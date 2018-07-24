import typing_extensions as tx
import typing as t


class P(tx.Protocol):
    start: int
    end: int


class Impl:
    def __init__(self, *, start: int, end: int) -> None:
        self.start = start
        self.end = end


def f(ob: P) -> str:
    return '({start}, {end})'.format(start=ob.start, end=ob.end)


def main() -> None:
    ob = Impl(start=10, end=20)
    print(f(ob))
