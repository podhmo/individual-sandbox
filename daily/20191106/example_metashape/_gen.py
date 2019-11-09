import typing as t
import dataclasses

from prestring.text import Module
from prestring.go import goname
from metashape.declarative import ignore
from metashape.analyze import typeinfo
from metashape.analyze import ModuleWalker


def detect_gotype(info: typeinfo.TypeInfo) -> str:
    if isinstance(info, typeinfo.Container):
        if info.container in ("list", "tuple", "set"):
            assert len(info.args) == 1
            return f"[]{detect_gotype(info.args[0])}"
        elif info.container == "dict":
            assert len(info.args) == 2
            return f"map[{detect_gotype(info.args[0])}]{detect_gotype(info.args[1])}"
    else:  # Atom
        typ = info.underlying
        if issubclass(typ, str):
            return "string"
        elif issubclass(typ, bool):
            return "bool"
        elif issubclass(typ, int):
            return "int"  # todo: int64 or ... something
        elif issubclass(typ, float):
            return "float"
    return goname(info.normalized.__name__)


@ignore
@dataclasses.dataclass
class Context:
    package: str = "gen"

    w: ModuleWalker = None
    m: Module = None


def emit_simple_type_definition(ctx: Context, cls: t.Type[t.Any], *, m=None) -> None:
    """
    e.g.
// Team ...
type Team struct {
      Name string `json:"name"`
      Members [][]Person `json:"members"`
}
    """
    m = m or ctx.m
    typename = goname(cls.__name__)
    m.stmt(f"// {typename} ...")
    m.stmt(f"type {typename} struct {{")
    with m.scope():
        for name, info, metata in ctx.w.for_type(cls).walk():
            m.stmt(f"""{goname(name)} {detect_gotype(info)} `json:"{name}"`""")
    m.stmt("}")
