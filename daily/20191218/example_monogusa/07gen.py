import typing as t
from functools import partial
from prestring.naming import pascalcase
import codeobject
from codeobject import Module
import fnspec


def schema_code_from_spec(m: Module, name: str, *, spec: fnspec.FuncSpec) -> Module:
    with m.class_(name, "BaseModel"):
        if spec.doc is not None:
            m.docstring(f"auto generated class from {spec.__module__}.{spec.name}")

        if len(spec.parameters) == 0:
            m.stmt("pass")

        for name, typ in spec.parameters:
            kind = spec.kind_of(name)
            if kind == "args":
                continue
            elif kind == "args_defaults":
                continue
            elif kind == "kw":
                m.stmt("{}: {}", name, spec.type_str_of(typ))
            elif kind == "kw_defaults":
                m.stmt(
                    "{}: {}  = {}", name, spec.type_str_of(typ), spec.default_of(name)
                )
            else:
                raise ValueError(f"invalid kind. name={name}, kind={kind}")
    return m


def view_code_from_spec(
    m: Module,
    name: str,
    *,
    spec: fnspec.FuncSpec,
    InputSchema: t.Optional[codeobject.Object],
) -> Module:
    # TODO: DI
    m.stmt('@router.post("/{}", response_model=runtime.CommandOutput)', spec.name)

    args = []
    if InputSchema is not None:
        args.append(f"input: {InputSchema}")

    with m.def_(name, *args, return_type="t.Dict[str, t.Any]"):
        if spec.doc is not None:
            m.docstring(spec.doc)
        with m.with_("runtime.handle() as s"):
            m.stmt("{}(**input.dict())", spec.fullname)
            m.stmt("return s.dict()")
    return m


def hello(*, name: str) -> None:
    """hello world"""
    print(f"hello, {name}")


def byebye() -> None:
    print("byebye")


def main():

    m = Module(import_unique=True)
    m.toplevel = m.submodule()
    m.sep()

    m.toplevel.import_("typing", as_="t")
    m.toplevel.from_("pydantic", "BaseModel")
    m.toplevel.from_("fastapi", "APIRouter", "Depends")

    m.stmt("router = APIRouter()")
    m.sep()

    for fn in [hello, byebye]:
        spec = fnspec.fnspec(fn)

        m.toplevel.from_("monogusa.web", "runtime")
        if fn.__module__ != "__main__":
            m.toplevel.import_(fn.__module__)

        InputSchema = None
        if len(spec.parameters) > 0:
            InputSchema = codeobject.codeobject(
                partial(schema_code_from_spec, spec=spec), name=pascalcase(fn.__name__)
            )
            m.stmt(InputSchema)

        view_callable = codeobject.codeobject(
            partial(view_code_from_spec, spec=spec, InputSchema=InputSchema),
            name=spec.name,
        )
        m.stmt(view_callable)

    with m.def_("main", return_type=None):
        m.from_("monogusa.web", "cli")
        m.from_("fastapi", "FastAPI")
        m.stmt("app = FastAPI()")
        m.stmt("app.include_router(router)")
        m.stmt("cli.run(app)")

    print(m)


if __name__ == "__main__":
    main()
