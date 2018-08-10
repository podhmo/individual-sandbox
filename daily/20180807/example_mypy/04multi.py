import typing as t
import mypy_extensions as mx

x0 = ()  # type: t.Tuple
x1 = (1, )  # type: t.Tuple[int]
x2 = (1, 2)  # type: t.Tuple[int, int]

y0 = ()  # type: t.Tuple[int, ...]
y1 = (1, )  # type: t.Tuple[int, ...]
y2 = (1, 2)  # type: t.Tuple[int, ...]


def f0() -> t.List[int]:
    return []


def f1(x0: int) -> t.List[int]:
    return [x0]


def f2(x0: int, x1: int) -> t.List[int]:
    return [x0, x1]


def fN(*xs: int) -> t.List[int]:
    return list(xs)


def g(f: t.Callable[[mx.VarArg(int)], t.List[int]], *xs: int) -> t.List[int]:
    return f(*xs)


print(g(fN, 10, 20))


def h0(x: int) -> t.Tuple:
    return ()


def h1(x: int) -> t.Tuple[int, ]:
    return (x, )


def h2(x: int) -> t.Tuple[int, int]:
    return (x, x)


def hN(x: int) -> t.Tuple[int, ...]:
    return (x, x, x, x)


def kN(x: int) -> t.Sequence[int]:
    return (x, x, x, x)


def kM(x: int) -> t.Sequence[int]:
    return [x, x, x, x]


