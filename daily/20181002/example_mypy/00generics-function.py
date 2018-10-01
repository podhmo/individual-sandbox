import typing as t

T = t.TypeVar("T")


def triple(x: T) -> t.Tuple[T, T, T]:
    return (x, x, x)


xs: t.Tuple[int, int, int] = triple(10)
ys: t.Tuple[str, str, str] = triple("x")
