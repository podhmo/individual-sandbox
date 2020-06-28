from __future__ import annotations
import typing as t
from dictknife import loading
from handofcats import as_command
from prestring.python import PythonModule as Module


def gen(
    d: t.Dict[str, t.Any], *, m: t.Optional[Module] = None, indent: str = "    "
) -> None:
    m = m or Module(indent=indent)

    m.from_("__future__").import_("annotations")
    t_pkg = m.import_("typing", as_="t")
    tx_pkg = m.import_("typing_extensions", as_="tx")
    m.sep()

    return m


# TODO
# - generate schema class definition
# - generate router function definition
# - separated output


@as_command
def run(*, filename: str) -> None:
    d = loading.loadfile(filename)
    m = Module()
    m = gen(d, m=m, indent="    ")
    print(m)
