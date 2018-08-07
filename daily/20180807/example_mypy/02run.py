from collections import defaultdict
import typing as t


def create() -> t.Tuple[t.Callable[[t.List[int], int], int], int, int]:
    return action, 10, 20


def action(xs: t.List[int], y: int) -> int:
    n = 0
    for i, x in enumerate(xs):
        n += x ** i
    return n


def run0() -> None:
    d = defaultdict(list)  # type: t.DefaultDict[t.Callable[[t.List[int], int], int], t.List[int]]
    ac, x, y = create()
    d[ac].append(x)
    ac, x, y = create()
    d[ac].append(x)
    ac, x, y = create()
    d[ac].append(x)
    print(ac(d[ac], y))


run0()
