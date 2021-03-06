import typing as t
from monogusa.web.codegen._fnspec import fnspec, Fnspec
from monogusa.web.codegen._codeobject import Object, codeobject, Module
from prestring.utils import LazyFormat
from prestring.naming import pascalcase


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


def g(
    name: str,
    val: int,
    default: int = 0,
    *,
    nickname: t.Optional[str] = None,
    debug: bool = False,
    children: t.List[str],
    **metadata: t.Optional[t.Any],
) -> None:
    pass


def create_class_code(spec: Fnspec) -> Object:
    """
def f(x:int, y:int, *, z:int=0) -> None:
    pass

to

class X:
    x: int
    y: int
    z: int = 0
    """

    @codeobject
    def code(m: Module, name: str) -> Module:
        with m.class_(name):
            for name, typ, kind in spec.parameters:
                if typ.__module__ != "builtins":
                    m.toplevel.import_(
                        typ.__module__, as_=spec._aliases.get(typ.__module__)
                    )

                if kind == "var_kw":
                    rhs = spec.type_str_of(t.Dict[str, typ])
                elif kind == "var_args":
                    rhs = spec.type_str_of(t.List[typ])
                else:
                    rhs = spec.type_str_of(typ)

                if kind == "kw_defaults" or kind == "args_defaults":
                    rhs = LazyFormat("{} = {}", rhs, spec.default_str_of(name))

                m.stmt("{}: {}  # {}", name, rhs, kind)
        return m

    code.name = pascalcase(spec.name)
    return code


m = Module()
m.toplevel = m.submodule(import_unique=True)
m.sep()

# todo: alias
for fn in [f, g]:
    spec = fnspec(fn)
    code = create_class_code(spec)

    m.toplevel.import_("dataclasses")
    m.stmt("@dataclasses.dataclass")
    m.stmt(code)
print(m)
