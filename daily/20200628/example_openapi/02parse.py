from __future__ import annotations
import typing as t
from dictknife import loading
from handofcats import as_command
from codegen import Module, Resolver, DefinitionGenerator


def gen(
    d: t.Dict[str, t.Any], *, m: t.Optional[Module] = None, indent: str = "    "
) -> None:
    m = m or Module(indent=indent)
    resolver = Resolver(m=m)
    if not hasattr(m, "toplevel"):
        m.toplevel = m.submodule()
    m.toplevel.from_("__future__").import_("annotations")
    m.toplevel.import_("typing", as_="t")
    m.toplevel.import_("typing_extensions", as_="tx")
    m.sep()

    # definitions
    if "definitions" in d:
        g = DefinitionGenerator(d["definitions"], m=m, resolver=resolver)
        for name, def_dict in d["definitions"].items():
            g.generate(name, def_dict)
    return m


# TODO
# - generate schema class definition
#   - support allOf
#   - support ref
#   - support anonymous definition
#   - support array's validation
# - generate router function definition
# - separated output


@as_command
def run(*, filename: str) -> None:
    d = loading.loadfile(filename)
    m = Module()
    m = gen(d, m=m, indent="    ")
    print(m)
