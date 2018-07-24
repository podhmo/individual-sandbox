import typing as t


class X:
    x: int

    def __init__(self, x: int) -> None:
        self.x = x


def f(x: X) -> None:
    print(x.x())


class Y:
    @property
    def y(self) -> int:
        return 0


def g(y: Y) -> None:
    print(y.y())


T = t.TypeVar("T")


class myproperty(t.Generic[T]):
    def __init__(self, fn: t.Callable[[t.Any], T]) -> None:
        self.fn = fn

    def __get__(self, ob: t.Any, typ: t.Any = None) -> T:
        return self.fn(ob)


class Z:
    @myproperty
    def z(self) -> int:
        return 0


def h(z: Z) -> None:
    print(z.z())


# mypy --strict 03*.py
# 03property.py:10: error: "int" not callable
# 03property.py:20: error: "int" not callable
# 03property.py:41: error: "int" not callable

