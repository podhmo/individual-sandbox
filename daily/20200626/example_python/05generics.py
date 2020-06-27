import typing as t
import typing_extensions as tx
from egoist.internal._fnspec import fnspec
from dataclasses import dataclass

T = t.TypeVar("T")


class Query(t.Generic[T]):
    @classmethod
    def __emit__(cls, name: str) -> None:
        print("emit", name)


@dataclass
class Name:
    name: str


s = Query[str]
print(s)
print(type(s))
print(vars(tx.Annotated[Query[str], Name("s")]))


def hello(name: tx.Annotated[Query[str], Name("s")]) -> None:
    pass


print(fnspec(hello).arguments[0][1].__metadata__[0].name)
