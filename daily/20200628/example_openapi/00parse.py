from __future__ import annotations
import typing as t
from dictknife import loading
from handofcats import as_command
from prestring.python.codeobject import Module


def gen(
    d: t.Dict[str, t.Any], *, m: t.Optional[Module] = None, indent: str = "    "
) -> None:
    m = m or Module(indent=indent)
    resolver = Resolver()

    m.from_("__future__").import_("annotations")
    t_pkg = m.import_("typing", as_="t")
    tx_pkg = m.import_("typing_extensions", as_="tx")
    m.sep()

    # definitions
    if "definitions" in d:
        g = DefinitionGenerator(d["definitions"], m=m, resolver=resolver)
        for name, def_dict in d["definitions"].items():
            g.generate(name, def_dict)
    return m


class Resolver:
    # todo: customizable
    def resolve_pytype(self, d: t.Dict[str, t.Any]) -> str:
        d.get("type")


class DefinitionGenerator:
    def __init__(
        self, definitions: t.Dict[str, t.Any], *, m: Module, resolver: Resolver
    ) -> None:
        self.definitions = definitions
        self.resolver = resolver
        self.m = m

    def generate(self, name: str, d: t.Dict[str, t.Any]) -> None:
        if "$ref" in d:
            return  # todo
        type_ = d.get("type", "object")
        if type_ == "object":
            return self._generate_object(name, d)
        elif type_ == "array":
            return  # todo
        else:
            return  # todo

    def _generate_object(self, name: str, d: t.Dict[str, t.Any]) -> None:
        m = self.m

        required_set = set(d.get("required") or [])
        with m.class_(name):
            if not d.get("properties"):
                m.stmt("pass")
                return

            for name, prop in d["properties"].items():
                m.stmt(f"{name}: str")


# TODO
# - generate schema class definition
#   - type mapping
#   - support optional
# - generate router function definition
# - separated output


@as_command
def run(*, filename: str) -> None:
    d = loading.loadfile(filename)
    m = Module()
    m = gen(d, m=m, indent="    ")
    print(m)
