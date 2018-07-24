from collections import ChainMap
import typing_extensions as tx
import typing as t


class P(tx.Protocol):
    start: int
    end: int


T = t.TypeVar("T", bound="StackedContext")


class StackedContext:
    cmap: ChainMap

    def __init__(self, cmap: t.Optional[ChainMap] = None) -> None:
        self.cmap = cmap or ChainMap({})

    def new_child(self: T, d: t.Optional[dict] = None) -> T:
        d = d or {}
        ob = self.__class__(self.cmap.new_child(d))  # type: T
        return ob

    def __getattr__(self, name: str) -> t.Any:
        try:
            return self.cmap[name]
        except KeyError as e:
            raise AttributeError(str(e))


def Impl(start: int, end: int) -> P:
    c = StackedContext()
    d = {"start": start, "end": end}
    return c.new_child(d)


def f(ob: P) -> str:
    return '({start}, {end})'.format(start=ob.start, end=ob.end)


def main() -> None:
    ob = Impl(start=10, end=20)
    print(f(ob))


if __name__ == '__main__':
    main()
