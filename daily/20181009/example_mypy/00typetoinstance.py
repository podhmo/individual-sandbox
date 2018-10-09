import typing as t

T = t.TypeVar("T")


def type_to_instance(typ: t.Type[T]) -> T:
    return typ()


class A:
    pass


def use(a: A) -> None:
    print(a)


def main() -> None:
    a = type_to_instance(A)
    use(a)
