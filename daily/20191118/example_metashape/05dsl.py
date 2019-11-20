import typing as t
import dataclasses
from prestring.text import Module


@dataclasses.dataclass(frozen=True)
class Choice:
    label: str
    name: str


@dataclasses.dataclass(frozen=True)
class Enum:
    name: str
    description: str
    choices: t.List[Choice]


class EnumPrinter:
    def __init__(self, m):
        self.m = m

    def definition(self, enum: Enum, *, m=None):
        m = m or self.m
        m.stmt(f"{enum.name}:")
        with m.scope():
            m.stmt(f"description: {enum.description}")
            m.stmt("type: string")
            m.stmt("enum:")
            with m.scope():
                for c in enum.choices:
                    m.stmt(f"- {c.name}")
            m.stmt("x-ja-enum:")
            with m.scope():
                for c in enum.choices:
                    m.stmt(f"- {c.label}")

    def parameter(self, enum: Enum, *, m=None):
        m = m or self.m
        m.stmt(f"{enum.name}:")
        with m.scope():
            m.stmt(f"name: {enum.name}")
            m.stmt("in: query")
            m.stmt(f"description: {enum.description}")
            m.stmt("type: string")
            m.stmt("enum:")
            with m.scope():
                for c in enum.choices:
                    m.stmt(f"- {c.name}")
            m.stmt("x-ja-enum:")
            with m.scope():
                for c in enum.choices:
                    m.stmt(f"- {c.label}")


class DSL:
    def __init__(self, m):
        self.m = m

    @property
    def enum(self):
        return EnumPrinter(self.m)

    def definitions(self, *, m=None):
        m = m or self.m
        m.stmt("definitions:")
        return m.scope()

    def parameters(self, *, m=None):
        m = m or self.m
        m.stmt("parameters:")
        return m.scope()


if __name__ == "__main__":
    season = Enum(
        name="season",
        description="四季",
        choices=[
            Choice(label="春", name="spring"),
            Choice(label="夏", name="summer"),
            Choice(label="秋", name="autumn"),
            Choice(label="冬", name="winter"),
        ],
    )

    m = Module(indent="  ")
    dsl = DSL(m)

    with dsl.definitions():
        dsl.enum.definition(season)
    m.sep()
    with dsl.parameters():
        dsl.enum.parameter(season)

    print(m)
