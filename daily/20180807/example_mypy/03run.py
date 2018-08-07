from collections import defaultdict
import typing as t
import mypy_extensions as mx


def create0() -> t.Tuple[t.Callable[[t.List[int], mx.VarArg(int)], int], int, int]:
    return action0, 10, 20


def action0(xs: t.List[int], *ys: int) -> int:
    n = 0
    for i, x in enumerate(xs):
        n += x ** i * ys[0]
    return n


def create1() -> t.Tuple[t.Callable[[t.List[int], mx.VarArg(int)], int], int]:
    return action1, 20


def action1(xs: t.List[int], *ys: int) -> int:
    n = 0
    for i, x in enumerate(xs):
        n += x ** i
    return n


def run() -> None:
    d = defaultdict(
        list
    )  # type: t.DefaultDict[t.Callable[[t.List[int], mx.VarArg(t.Any)], int], t.List[int]]
    ac, x, y = create0()
    d[ac].append(x)
    ac, x = create1()
    d[ac].append(x)
    ac, x, y = create0()
    d[ac].append(x)
    print(ac(d[ac], y))


run()
