import typing as t
from functools import partial
from handofcats import as_command

from _gen import g
from prestring.output import output
from prestring.text import Module
from prestring.naming import snakecase
from metashape.runtime import get_walker


class Person:
    name: str
    age: int


class Team:
    name: str
    members: t.List[t.List[Person]]


@as_command
def run(dst: str):
    w = get_walker(aggressive=True, here=__name__)

    with output(
        dst, use_console=True, verbose=True, opener=partial(Module, indent="\t")
    ) as fs:
        for cls in w.walk():
            with fs.open(f"{snakecase(cls.__name__)}.go", "w") as m:
                ctx = g.Context(package="gen", w=w, m=m)
                m.stmt(f"package {ctx.package}")
                m.sep()
                g.emit_simple_type_definition(ctx, cls)
