import typing as t
import typing_extensions as tx
T = t.TypeVar("T")


class HasName(tx.Protocol):
    name: str


def type_to_instance(typ: t.Type[T]) -> T:
    return typ()


class A:
    name: str


def use(x: HasName) -> None:
    print(x.name)


def main() -> None:
    a = type_to_instance(A)
    use(a)
