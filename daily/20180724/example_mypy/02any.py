import typing as t


def f() -> t.Any:
    return "foo"


def g(x: str) -> None:
    print(f"{x} ({type(x)})")


def h(x: int) -> None:
    print(f"{x} ({type(x)})")


g(f())
h(f())  # hmm
