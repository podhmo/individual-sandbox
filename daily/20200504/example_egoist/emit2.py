import typing as t
from walker import walk
from prestring.go.codeobject import gofile, Module
from prestring.go import goname
from egoist.go.resolver import get_resolver
from metashape.declarative import field  # noqa: F401
from walker import walk, metadata, Item  # noqa: F401


def emit(classes: t.List[t.Type[t.Any]]) -> Module:
    m = gofile("main")
    r = get_resolver(m)

    for item in walk(classes):
        this = m.symbol(f"{item.type_.__name__[0].lower()}")
        this_type = f"{r.resolve_gotype(item.type_)}"
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

                    gotype: str = r.resolve_gotype(typeinfo.raw)
                    m.append(f'{goname(name)} *{gotype} `json:"{name}"`')
                    m.stmt("// required" if metadata["required"] else "")
            m.stmt("}")

            # if rawErr := json.Unmarshal(b, &inner); rawErr != nil {
            # ...
            # }
            json_pkg = m.import_("encoding/json")
            raw_err = m.symbol("rawErr")
            with m.if_(
                f"{raw_err} := {json_pkg}.Unmarshal(b, &{inner}); {raw_err} != nil"
            ):
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
    return m
