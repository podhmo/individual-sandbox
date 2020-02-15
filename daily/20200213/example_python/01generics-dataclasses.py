import typing as t
import dataclasses

T = t.TypeVar("T")


@dataclasses.dataclass
class X(t.Generic[T]):
    name: str
    raw: T


def foo(x: X[str]) -> None:
    print(x.raw.upper())


print(X(name="foo", raw="xxx"))
