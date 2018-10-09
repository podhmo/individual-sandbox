import typing as t
import mypy_extensions as mx


class User(mx.TypedDict):
    id: str
    name: str
    age: int
    group_id: str


class Named(mx.TypedDict):
    name: str


T = t.TypeVar("T", bound=Named)


# def zero(typ: t.Type[T]) -> t.Sequence[T]:
def zero(typ: t.Callable[..., T]) -> t.Sequence[T]:
    return []


def use(xs: t.Sequence[Named]) -> None:
    for x in xs:
        print(x["name"])
