import typing as t
import typing_extensions as tx
import typing_inspect
from prestring.text import Module


class Labels:
    def __init__(self, *labels: str) -> None:
        self.labels = labels


class _String:
    def __init__(self, name: str) -> None:
        self.name = name

    def __str__(self):
        return self.name


class Name(_String):
    pass


class Description(_String):
    pass


def get_name(typ: t.Type[t.Any]) -> str:
    # see: __name__
    return get_metadata(typ).get(Name)


def get_description(typ: t.Type[t.Any]) -> str:
    # see: docstring
    return get_metadata(typ).get(Description)


def get_metadata(typ: t.Type[t.Any]) -> t.Dict[t.Type[t.Any], t.Any]:
    candidates = getattr(typ, "__metadata__") or {}
    return {x.__class__: x for x in candidates}


def get_args(typ: t.Type[t.Any]) -> t.List[t.Any]:
    if hasattr(typ, "__metadata__"):
        typ = typing_inspect.get_args(typ)[0]
    return typing_inspect.get_args(typ)


Season = tx.Annotated[
    tx.Literal["spring", "summer", "autumn", "winter"],
    Labels("春", "夏", "秋", "冬"),
    Name("Season"),
    Description("四季の分類"),
]


def gen(typ: t.Type[t.Any], *, m=None, indent="    "):
    m = m or Module(indent=indent)
    def_name = get_name(typ)
    def_description = get_description(typ)

    m.stmt("definitions:")
    with m.scope():
        m.stmt(f"{def_name}:")
        with m.scope():
            m.stmt(f"description: {def_description}")
            m.stmt("type: string")
            m.stmt("enum:")
            with m.scope():
                for name in get_args(typ):
                    m.stmt(f"- {name}")

            metadata = get_metadata(typ)
            if Labels in metadata:
                m.stmt("x-ja-enum:")
                with m.scope():
                    for name in metadata[Labels].labels:
                        m.stmt(f"- {name}")
        m.sep()
    m.stmt("parameters:")
    with m.scope():
        m.stmt(f"{def_name}:")
        with m.scope():
            m.stmt(f"name: {name}")
            m.stmt("in: query")
            m.stmt(f"description: {def_description}")
            m.stmt("type: string")
            m.stmt("enum:")
            with m.scope():
                for name in get_args(typ):
                    m.stmt(f"- {name}")

            metadata = get_metadata(typ)
            if Labels in metadata:
                m.stmt("x-ja-enum:")
                with m.scope():
                    for name in metadata[Labels].labels:
                        m.stmt(f"- {name}")
    return m


if __name__ == "__main__":
    m = gen(Season, indent="    ")
    print(m)
