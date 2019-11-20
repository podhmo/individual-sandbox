import typing as t
import dataclasses
from prestring.text import Module


@dataclasses.dataclass(frozen=True)
class Category:
    ja_name: str
    en_name: str


@dataclasses.dataclass(frozen=True)
class Data:
    ja_name: str
    en_name: str
    categories: t.List[Category]


def gen(data: Data, *, m=None, indent="    "):
    m = m or Module(indent=indent)

    m.stmt("definitions:")
    with m.scope():
        m.stmt(f"{data.en_name}:")
        with m.scope():
            m.stmt(f"description: {data.ja_name}")
            m.stmt("type: string")
            m.stmt("enum:")
            with m.scope():
                for c in data.categories:
                    m.stmt(f"- {c.en_name}")
            m.stmt("x-ja-enum:")
            with m.scope():
                for c in data.categories:
                    m.stmt(f"- {c.ja_name}")
        m.sep()
    m.stmt("parameters:")
    with m.scope():
        m.stmt(f"{data.en_name}:")
        with m.scope():
            m.stmt(f"name: {data.en_name}")
            m.stmt("in: query")
            m.stmt(f"description: {data.ja_name}")
            m.stmt("type: string")
            m.stmt("enum:")
            with m.scope():
                for c in data.categories:
                    m.stmt(f"- {c.en_name}")
            m.stmt("x-ja-enum:")
            with m.scope():
                for c in data.categories:
                    m.stmt(f"- {c.ja_name}")
    return m


if __name__ == "__main__":
    data = Data(
        en_name="season",
        ja_name="四季",
        categories=[
            Category(ja_name="春", en_name="spring"),
            Category(ja_name="夏", en_name="summer"),
            Category(ja_name="秋", en_name="autumn"),
            Category(ja_name="冬", en_name="winter"),
        ],
    )
    m = gen(data, indent="    ")
    print(m)
