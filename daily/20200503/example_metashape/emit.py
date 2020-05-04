from __future__ import annotations
import typing as t
import inspect
from prestring.go.codeobject import Module, gofile
from prestring.go import goname
from prestring.naming import untitleize
from egoist.go.resolver import get_resolver, Resolver
from egoist.go.types import get_gopackage
from metashape.declarative import field  # noqa: F401
from metashape.analyze import typeinfo
from walker import walk, metadata, Item  # noqa: F401


def build_gotags(tags: t.Dict[str, t.List[str]]) -> str:
    return " ".join(f'''{k}:"{', '.join(vs)}"''' for k, vs in tags.items())


def has_class_object(info: typeinfo.TypeInfo) -> bool:
    if not hasattr(info, "args"):  # xxx
        return typeinfo.get_custom(info) is not None
    return any(typeinfo.get_custom(sinfo) is not None for sinfo in info.args)


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
        for name, info, metadata in item.fields:
            gotype: str = resolver.resolve_gotype(info.raw)

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

        # var err *maperr.Error
        err = m.symbol("err")
        maperr_pkg = m.import_("github.com/podhmo/maperr")
        m.stmt(f"var {err} *{maperr_pkg}.Error")
        m.sep()

        # var inner struct {
        #   ...
        # }
        m.stmt("// loading internal data")
        inner = m.symbol("inner")
        m.stmt(f"var {inner} struct {{")
        with m.scope():
            for name, info, metadata in item.fields:
                if name.startswith("_"):
                    continue  # xxx:

                if has_class_object(info):
                    json_pkg = m.import_("encoding/json")
                    gotype: str = str(json_pkg.RawMessage)
                else:
                    gotype: str = resolver.resolve_gotype(info.raw)

                m.append(f'{goname(name)} *{gotype} `json:"{name}"`')
                m.stmt("// required" if metadata["required"] else "")
        m.stmt("}")

        # if rawErr := json.Unmarshal(b, &inner); rawErr != nil {
        # ...
        # }
        json_pkg = m.import_("encoding/json")
        raw_err = m.symbol("rawErr")
        with m.if_(f"{raw_err} := {json_pkg}.Unmarshal(b, &{inner}); {raw_err} != nil"):
            m.return_(err.AddSummary(raw_err.Error()))
        m.sep()

        # if <field> != nil {
        #     ob.<field> = *<field>
        # } else {
        #     m.add(<field>, "required")
        # }
        rawerr = m.symbol("rawerr")
        m.stmt("// binding field value and required check")
        for name, info, metadata in item.fields:
            field = m.symbol(goname(name))
            with m.if_(f"{inner}.{field} != nil"):
                if has_class_object(info):
                    # pointer
                    if info.is_optional:
                        gotype: str = resolver.resolve_gotype(info.normalized)
                        m.stmt(f"{this}.{goname(name)} = &{gotype}{{}}")
                        ref = f"{this}.{field}"
                    elif hasattr(info, "args"):  # xxx
                        gotype: str = resolver.resolve_gotype(info.normalized)
                        m.stmt(f"{this}.{goname(name)} = {gotype}{{}}")
                        ref = f"&{this}.{field}"
                    else:
                        ref = f"&{this}.{field}"

                    with m.if_(
                        f"{rawerr} := json.Unmarshal(*{inner}.{field}, {ref}); {rawerr} != nil"
                    ):
                        m.stmt(
                            f'{err} = {err}.Add("{name}", {maperr_pkg}.Message{{Error: {rawerr}}})'
                        )
                else:
                    m.stmt(f"{this}.{field} = *{inner}.{field}")
            if metadata["required"]:
                with m.else_():
                    m.stmt(
                        f'{err} = err.Add("{name}", {maperr_pkg}.Message{{Text: "required"}})'
                    )
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

    with m.func("main"):
        pass

    return m


_NEWLINE = "\n"
