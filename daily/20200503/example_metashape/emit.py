from __future__ import annotations
import typing as t
import inspect
import dataclasses
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


@dataclasses.dataclass(frozen=True)
class Definition:
    name: str
    code_module: t.Optional[Module]


def emit_struct(m: Module, item: Item, *, resolver: Resolver) -> Definition:
    gopackage = get_gopackage(item.type_)
    if gopackage is not None:
        return ""

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

    definition = Definition(name=typename, code_module=None)
    m.stmt("}")
    return definition


def emit_union(m: Module, item: Item, *, resolver: Resolver) -> Definition:
    typename = goname(item.type_.__name__)
    kind_typename = typename + "Kind"

    # type <typename> {
    #     Kind string `json:"$kind"`
    # ...
    # }
    m.stmt(f"type {typename} struct {{")
    with m.scope():
        m.stmt(f'Kind {kind_typename} `json:"$kind"`')
        for subtype in item.args:
            gotype: str = resolver.resolve_gotype(subtype)
            m.append(f"{gotype} *{gotype}")
            m.stmt(f' `json:"{untitleize(str(gotype)).rstrip("_")},omitempty"`')

    m.stmt("}")
    m.sep()

    # UnmarshalJSON
    discriminator_field = ("$kind", typeinfo.typeinfo(str), metadata())
    discriminator_field[-1]["_override_type"] = kind_typename

    pseudo_fields = [
        (sub_type.__name__, typeinfo.typeinfo(sub_type), metadata(required=False))
        for sub_type in item.args
    ]
    pseudo_item = Item(
        type_=item.type_, fields=[discriminator_field] + pseudo_fields, args=[],
    )

    unmarshalJSON_definition = emit_unmarshalJSON(m, pseudo_item, resolver=resolver)
    m.sep()

    # one-of validation
    assert unmarshalJSON_definition.code_module is not None
    this = m.symbol(f"{item.type_.__name__[0].lower()}")
    maperr_pkg = m.import_("github.com/podhmo/maperr")

    sm = unmarshalJSON_definition.code_module
    sm.stmt("// one-of?")
    sm.stmt("{")
    with sm.scope():
        for go_name, info, _ in pseudo_item.fields[1:]:
            with sm.if_(f'{this}.Kind == "{go_name}" && {this}.{go_name} == nil'):
                sm.stmt(
                    f'err = err.Add("{go_name}", {maperr_pkg}.Message{{Text: "treated as {go_name}, but no data"}})'
                )
    sm.stmt("}")

    # enums
    emit_enums(m, item.type_, resolver=resolver, name=kind_typename)

    definition = Definition(name=typename, code_module=None)
    return definition


def emit_enums(
    m: Module,
    literal_type: t.Type[t.Any],
    *,
    resolver: Resolver,
    name: t.Optional[str] = None,
) -> str:
    # literal_type or union_type
    go_type = name or f"{resolver.resolve_gotype(literal_type)}"

    first_of_args = t.get_args(literal_type)[0]
    base_go_type = resolver.resolve_gotype(
        type(getattr(first_of_args, "__name__", first_of_args))
    )

    const_names = [getattr(x, "__name__", x) for x in t.get_args(literal_type)]
    const_members = {name: f"{go_type}{goname(name)}" for name in const_names}
    this = m.symbol("v")
    as_literal = resolver.resolve_default

    # type <enum> string
    m.stmt(f"type {go_type} {base_go_type}")
    m.sep()

    # const (
    #     <enum>xxx <enum> = "xxx"
    # ...
    # )
    with m.const_group() as cg:
        for name in const_names:
            cg(f"{const_members[name]} {go_type} = {as_literal(type(name), name)}")
    m.sep()

    # func (v <enum>) Valid() error {
    # ...
    # }
    with m.method(f"{this} {go_type}", "Valid", returns="error"):
        with m.switch(this) as sm:
            with sm.case(", ".join(const_members.values())):
                sm.return_("nil")
            with sm.default() as sm:
                fmt_pkg = m.import_("fmt")
                candidates = ", ".join([str(x) for x in const_names])
                sm.return_(
                    fmt_pkg.Errorf(
                        as_literal(str, f"%q is invalid enum value of ({candidates})"),
                        this,
                    )
                )
        sm.unnewline()

    # func (v <enum>) UnmarshalJSON(b []byte) error {
    # ...
    # }
    with m.method(f"{this} *{go_type}", "UnmarshalJSON", f"b []byte", returns="error"):
        strings_pkg = m.import_("strings")
        m.stmt(f'*{this} = {go_type}({strings_pkg}.Trim(string(b), `"`))')
        m.return_(this.Valid())

    definition = Definition(name=go_type, code_module=None)
    return definition


def emit_unmarshalJSON(m: Module, item: Item, *, resolver: Resolver) -> Definition:
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

                if "_override_type" in metadata:
                    gotype: str = metadata["_override_type"]
                elif has_class_object(info):
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
        with m.block():
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

        # NOTE: for injecting code from extrnal area
        code_module = m.submodule("", newline=False)

        # return err.Untyped()
        m.return_(err.Untyped())

    m.stmt("}")
    return Definition(name="UnmarshalJSON", code_module=code_module)


def emit(classes: t.List[t.Type[t.Any]], *, name: str = "main") -> Module:
    m = gofile(name)
    r = get_resolver(m)

    for item in walk(classes):
        if item.is_union:
            emit_union(m, item, resolver=r)
            m.sep()
        else:
            emit_struct(m, item, resolver=r)
            m.sep()
            emit_unmarshalJSON(m, item, resolver=r)
            m.sep()

    with m.func("main"):
        pass

    return m


_NEWLINE = "\n"
