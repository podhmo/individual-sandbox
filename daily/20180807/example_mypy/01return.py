import typing as t


def f0(x: int) -> t.Tuple[int]:
    return (x, )


def f1(x: int) -> t.Tuple[int, int]:
    return (x, x)


def f2(x: int) -> t.Tuple[int, int, int]:
    return (x, x, x)


# def f3(x: int) -> tuple:
def f3(x: int) -> t.Tuple[int, ...]:
    return (x, x, x, x)


def call(f: t.Callable[[int], t.Tuple[int, ...]], x: int) -> t.Tuple[int, ...]:
    return f(x)
