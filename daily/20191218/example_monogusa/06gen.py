import typing as t
from functools import partial
from prestring.naming import pascalcase
import codeobject
from codeobject import Module
import fnspec


def schema_code_from_callable(m: Module, name: str, *, fn: t.Callable[..., t.Any]):
    spec = fnspec.fnspec(fn)
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


def hello(*, name: str) -> None:
    """hello world"""
    print(f"hello, {name}")


@codeobject.codeobject
def Hello(m: Module, name: str) -> Module:
    return schema_code_from_callable(m, name, fn=hello)


@codeobject.codeobject
def hello_with_hello(m: Module, name: str) -> Module:
    with m.def_(name, return_type=Hello):
        p = m.let("p", Hello(name="foo", age=20))
        m.stmt(p.say("hello"))  # type: ignore
        m.return_(p)
    return m


@codeobject.codeobject
def hello_view(m: Module, name: str) -> Module:
    m.toplevel.from_("monogusa.web", "runtime")  # todo: toplevel?

    m.stmt('@router.post( "/{}", response_model=runtime.CommandOutput)', hello.__name__)
    with m.def_(name, f"input: {hello.__name__}", return_type=Hello):
        if hello.__doc__ is not None:
            m.docstring(hello.__doc__)
        with m.with_("runtime.handle() as s"):
            m.stmt("{}.{}(**input.dict())", "cli", hello.__name__, hello.__name__)
            m.stmt("return s.dict()")
    return m


m = Module()
m.toplevel = m.submodule()
m.sep()
m.stmt(Hello)
m.stmt(hello_with_hello)
m.stmt(hello_view)
print(m)
