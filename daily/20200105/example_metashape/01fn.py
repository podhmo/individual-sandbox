import typing as t
from monogusa.web.codegen._fnspec import fnspec
from metashape.analyze.typeinfo import typeinfo
from prestring.python import Module, LazyFormat


def f(
    name: str,
    val: int,
    default: int = 0,
    *,
    nickname: t.Optional[str] = None,
    debug: bool = False,
    **metadata: t.Optional[t.Any],
) -> None:
    pass


m = Module()
m.toplevel = m.submodule(import_unique=True)
m.sep()

spec = fnspec(f)
with m.class_("F"):
    for name, typ, kind in spec.parameters:
        if typ.__module__ != "builtins":
            m.toplevel.import_(typ.__module__)

        info = typeinfo(typ)
        rhs = spec.type_str_of(info.normalized)
        if info.is_optional:
            rhs = LazyFormat("typing.Optional[{}]", rhs)

        if kind == "var_kw":
            rhs = LazyFormat("typing.Dict[str, {}]", rhs)
        elif kind == "var_args":
            rhs = LazyFormat("typing.List[{}]", rhs)
        elif kind == "kw_defaults" or kind == "args_defaults":
            rhs = LazyFormat("{} = {}", rhs, spec.default_of(name))

        m.stmt("{}: {}", name, rhs)


print(m)
