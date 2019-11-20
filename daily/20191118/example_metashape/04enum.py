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

    def gen_definition(self, *, m):
        m.stmt(f"{self.name}:")
        with m.scope():
            m.stmt(f"description: {self.description}")
            m.stmt("type: string")
            m.stmt("enum:")
            with m.scope():
                for c in self.choices:
                    m.stmt(f"- {c.name}")
            m.stmt("x-ja-enum:")
            with m.scope():
                for c in self.choices:
                    m.stmt(f"- {c.label}")

    def gen_parameter(self, *, m):
        m.stmt(f"{self.name}:")
        with m.scope():
            m.stmt(f"name: {self.name}")
            m.stmt("in: query")
            m.stmt(f"description: {self.description}")
            m.stmt("type: string")
            m.stmt("enum:")
            with m.scope():
                for c in self.choices:
                    m.stmt(f"- {c.name}")
            m.stmt("x-ja-enum:")
            with m.scope():
                for c in self.choices:
                    m.stmt(f"- {c.label}")


def gen(data: Enum, *, m=None, indent="    "):
    m = m or Module(indent=indent)

    m.stmt("definitions:")
    with m.scope():
        data.gen_definition(m=m)
        m.sep()
    m.stmt("parameters:")
    with m.scope():
        data.gen_parameter(m=m)
    return m


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
    m = gen(season, indent="  ")
    print(m)
