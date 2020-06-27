import typing as t
from egoist.internal._fnspec import fnspec

T = t.TypeVar("T")


class Query(t.Generic[T]):
    pass


def hello(name: Query[str]):
    pass


print(fnspec(hello))
