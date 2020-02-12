import typing as t
from prestring.python import Module
from prestring.naming import titleize
from monogusa.web.codegen._fnspec import fnspec


def hello(
    name: str, *, age: int, nickname: t.Optional[str] = None
) -> t.Dict[str, t.Any]:
    pass


m = Module()
fn = hello
spec = fnspec(fn)
with m.class_(titleize(fn.__name__)):
    if len(spec.keyword_arguments) == 0:
        m.stmt("pass")
    for name, typ, kind in spec.parameters:
        if kind.endswith("defaults"):
            m.stmt(
                "{}: {} = {}", name, spec.type_str_of(typ), spec.default_str_of(name)
            )
        else:
            m.stmt("{}: {}", name, spec.type_str_of(typ))
print(m)
