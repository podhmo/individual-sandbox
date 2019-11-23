import typing as t

T = t.TypeVar("T")


class Marker(t.Generic[T]):
    def __init__(self, name: str, *, default: t.Optional[T] = None) -> None:
        self.name = name
        self.default = default

    def guess(self, ob: object) -> t.Optional[T]:
        return getattr(ob, self.name, self.default)  # type:ignore

    __call__ = guess

    def mark(self, value: object) -> t.Callable[[object], object]:
        def _wrapped(ob: object) -> object:
            setattr(ob, self.name, value)
            return ob

        return _wrapped


is_toplevel: Marker[bool] = Marker("mark_toplevel")


@is_toplevel.mark(True)
class A:
    name: str


print(is_toplevel(A))
