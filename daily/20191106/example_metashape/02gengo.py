from __future__ import annotations
import typing as t
import dataclasses
from functools import partial
from handofcats import as_command

from prestring.naming import snakecase
from prestring.go import goname
from metashape.runtime import get_walker
from metashape.analyze import typeinfo


class Person:
    name: str
    age: int


class Team:
    name: str
    members: t.List[t.List[Person]]


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


@dataclasses.dataclass
class Context:
    package: str = "gen"

    w: "ModuleWalker" = None
    m: "Module" = None


def emit_package_clause(ctx: Context, *, m=None) -> None:
    """
    e.g.
package gen
    """
    m = m or ctx.m
    m.stmt(f"package {ctx.package}")


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


@as_command
def run():
    import sys
    from prestring.output import output
    from prestring.text import Module

    m = sys.modules[__name__]
    w = get_walker(m.__dict__, aggressive=True)

    with output(
        "./02dst", use_console=True, verbose=True, opener=partial(Module, indent="\t")
    ) as fs:
        for cls in w.walk():
            if issubclass(cls, Context):
                continue

            with fs.open(f"{snakecase(cls.__name__)}.go", "w") as m:
                ctx = Context(package="gen", w=w, m=m)
                emit_package_clause(ctx)
                m.sep()
                emit_simple_type_definition(ctx, cls)
