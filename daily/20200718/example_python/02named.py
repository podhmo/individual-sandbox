import typing as t
import typing_extensions as tx

mapping = {}


def named(name: str, typ: t.Type[t.Any]) -> t.Type[t.Any]:
    global mapping
    mapping[typ] = name
    return typ


XXX = named("XXX", tx.Literal["x", "y", "z"])


def foo(xxx: XXX) -> None:
    pass
