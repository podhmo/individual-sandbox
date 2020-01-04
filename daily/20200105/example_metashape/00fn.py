import typing as t
from monogusa.web.codegen._fnspec import fnspec
from metashape.analyze.typeinfo import typeinfo
from prestring.python import Module, LazyFormat


def f(
    name: str, value: int = 0, *, nickname: t.Optional[str] = None, debug: bool = False
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
        type_str = spec.type_str_of(info.normalized)
        if info.is_optional:
            type_str = LazyFormat("typing.Optional[{}]", type_str)
        elif kind == "var_kw":
            type_str = LazyFormat("typing.Dict[str, {}]", type_str)
        elif kind == "var_args":
            type_str = LazyFormat("typing.List[{}]", type_str)
        m.stmt("{}: {}", name, type_str)

        if kind == "kw_defaults" or kind == "args_defaults":
            m.unnewline()
            m.stmt(" = {}", spec.default_of(name))


print(m)
