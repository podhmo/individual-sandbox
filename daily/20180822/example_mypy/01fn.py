import typing as t
F = t.Callable[[int, int], int]


def add(x: int, y: int, *, verbose: bool = False) -> int:
    return x + y


def add2(x: int, y: int, verbose: bool = True) -> int:
    return x + y


def use(f: F, x: int, y: int) -> int:
    return f(x, y)


def main() -> None:
    print(use(add, 10, 20))
    print(use(add2, 10, 20))
