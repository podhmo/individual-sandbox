from __future__ import annotations
import typing as t
import inspect
from prestring.go.codeobject import Module, gofile
from prestring.go import goname
from prestring.naming import untitleize
from egoist.go.resolver import get_resolver, Resolver
from egoist.go.types import get_gopackage
from metashape.declarative import field  # noqa: F401
from walker import walk, metadata, Item  # noqa: F401


def build_gotags(tags: t.Dict[str, t.List[str]]) -> str:
    return " ".join(f'''{k}:"{', '.join(vs)}"''' for k, vs in tags.items())


def emit_struct(m: Module, item: Item, *, resolver: Resolver) -> None:
    gopackage = get_gopackage(item.type_)
    if gopackage is not None:
        return

    typename = str(resolver.resolve_gotype(item.type_))

    # // <typename> ...
    doc = inspect.getdoc(item.type_)
    if doc:
        lines = doc.split("\n")
        m.stmt(f"// {typename} {lines[0]}")
        for line in lines[1:]:
            m.stmt(f"// {line}")

    # type <typename> struct {
    # ...
    # }
    m.stmt(f"type {typename} struct {{")
    with m.scope():
        for name, typeinfo, metadata in item.fields:
            gotype: str = resolver.resolve_gotype(typeinfo.raw)

            # handling field (private field?, embedded?)
            if metadata.get("inline", False):
                m.append(gotype)
            elif name.startswith("_"):
                m.append(f"{untitleize(goname(name))} {gotype}")
            else:
                m.append(f"{goname(name)} {gotype}")

            # todo: handling tags
            if "tags" not in metadata:
                metadata["tags"] = {}
            metadata["tags"] = {"json": [name.rstrip("_")]}
            m.append(f" `{build_gotags(metadata['tags'])}`")

            # handling comments
            if metadata.get("inline", False):
                m.stmt(f"  // {metadata}")
            else:
                comment = metadata.get("comment", "")
                m.stmt(f"  // {comment.split(_NEWLINE, 1)[0]}" if comment else "")

    m.stmt("}")


def emit_union(m: Module, item: Item, *, resolver: Resolver) -> None:
    typename = goname(item.type_.__name__)

    # type <typename> {
    #     Kind string `json:"$kind"`
    # ...
    # }
    m.stmt(f"type {typename} struct {{")
    with m.scope():
        m.stmt('Kind string `json:"$kind"`')
        for subtype in item.args:
            gotype: str = resolver.resolve_gotype(subtype)
            m.append(f"{gotype} *{gotype}")

            tags = {"json": [untitleize(str(gotype)).rstrip("_")]}
            m.stmt(f" `{build_gotags(tags)}`")

    m.stmt("}")


def emit_unmarshalJSON(m: Module, item: Item, *, resolver: Resolver) -> None:
    this = m.symbol(f"{item.type_.__name__[0].lower()}")
    this_type = f"{resolver.resolve_gotype(item.type_)}"
    this_type_pointer = f"*{this_type}"

    # func (ob *Ob) UnmarshalJSON(b []byte) error {
    b = m.symbol("b")
    m.stmt(f"func ({this} {this_type_pointer}) UnmarshalJSON({b} []byte) error {{")
    with m.scope():

        # var err *errmap.Error
        err = m.symbol("err")
        errmap_pkg = m.import_("github.com/podhmo/errmap")
        m.stmt(f"var {err} *{errmap_pkg}.Error")
        m.sep()

        # var inner struct {
        #   ...
        # }
        m.stmt("// loading internal data")
        inner = m.symbol("inner")
        m.stmt(f"var {inner} struct {{")
        with m.scope():
            for name, typeinfo, metadata in item.fields:
                if name.startswith("_"):
                    continue  # xxx:

                gotype: str = resolver.resolve_gotype(typeinfo.raw)
                m.append(f'{goname(name)} *{gotype} `json:"{name}"`')
                m.stmt("// required" if metadata["required"] else "")
        m.stmt("}")

        # if rawErr := json.Unmarshal(b, &inner); rawErr != nil {
        # ...
        # }
        json_pkg = m.import_("encoding/json")
        raw_err = m.symbol("rawErr")
        with m.if_(f"{raw_err} := {json_pkg}.Unmarshal(b, &{inner}); {raw_err} != nil"):
            m.return_(err.addSummary(raw_err.Error()))
        m.sep()

        # if <field> != nil {
        #     ob.<field> = *<field>
        # } else {
        #     m.add(<field>, "required")
        # }
        m.stmt("// binding field value and required check")
        for name, typeinfo, metadata in item.fields:
            field = m.symbol(goname(name))
            with m.if_(f"{inner}.{field} != nil"):
                m.stmt(f"{this}.{field} = *{inner}.{field}")
            if metadata["required"]:
                with m.else_():
                    m.stmt(f'{err} = err.Add("{name}", "required")')
        m.sep()

        # return err.Untyped()
        m.return_(err.Untyped())
    m.stmt("}")


def emit(classes: t.List[t.Type[t.Any]], *, name: str = "main") -> Module:
    m = gofile(name)
    r = get_resolver(m)

    for item in walk(classes):
        if item.is_union:
            emit_union(m, item, resolver=r)
            m.sep()
            # todo: unmarshalJSON
        else:
            emit_struct(m, item, resolver=r)
            m.sep()
            emit_unmarshalJSON(m, item, resolver=r)
            m.sep()

    return m


_NEWLINE = "\n"
