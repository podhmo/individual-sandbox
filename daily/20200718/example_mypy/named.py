import typing as t


def named(name: str, typ: t.Type[t.Any]) -> t.Type[t.Any]:
    return typ
