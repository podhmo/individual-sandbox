import typing as t


def f(g: t.Callable[[int], int], x: int) -> int:
    return g(x)
