import typing as t

Foo = t.NewType("Foo", str)


def use(x: str) -> None:
    ...


def use2(x: Foo) -> None:
    ...


def main() -> None:
    foo = Foo("foo")
    use(foo)
    use2(foo)
    use2("x")
