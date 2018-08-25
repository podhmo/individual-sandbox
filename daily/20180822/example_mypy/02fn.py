import typing as t
import mypy_extensions as mx
F = t.Callable[[int, int, mx.DefaultNamedArg(bool, "verbose")], int]


def add(x: int, y: int, *, verbose: bool = False) -> int:
    return x + y


def add2(x: int, y: int, verbose: bool = False) -> int:
    return x + y


def use(f: F, x: int, y: int) -> int:
    return f(x, y)


def main() -> None:
    print(use(add, 10, 20))
    print(use(add2, 10, 20))
