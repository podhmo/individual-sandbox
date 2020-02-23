import typing as t
import typing_extensions as tx

A = t.TypeVar("A", covariant=True)
B = t.TypeVar("B")


class HasEach(tx.Protocol[A]):
    def each(self) -> t.Iterator[A]:
        raise NotImplementedError("each")


class EnumerableMixin(HasEach[A]):
    def map(self, fn: t.Callable[[A], B]) -> t.List[B]:
        return [fn(x) for x in self.each()]


class List(EnumerableMixin[A]):
    def __init__(self, xs: t.List[A]) -> None:
        self.xs = xs

    # def each(self) -> t.Iterator[A]:
    #     return iter(self.xs)


L = List([10, 20, 30])
if t.TYPE_CHECKING:
    reveal_type(L)
result = L.map(lambda x: x * x)
if t.TYPE_CHECKING:
    reveal_type(result)
print(result)
