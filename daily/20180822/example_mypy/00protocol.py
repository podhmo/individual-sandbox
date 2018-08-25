import typing as t
import typing_extensions as tx


class A:
    def add(self, x: int, y: int, *, verbose: bool = False) -> int:
        return x + y


class B:
    def add(self, x: int, y: int, verbose: bool = False) -> int:
        return x + y


class C:
    def add(self, x: int, y: int) -> t.Any:
        return x + y


class Adder(tx.Protocol):
    def add(self, x: int, y: int) -> int:
        ...


def use(adder: Adder, x: int, y: int) -> int:
    return adder.add(x, y)


def main() -> None:
    a = A()
    print(use(a, 10, 20))
    b = B()
    print(use(b, 10, 20))
    c = C()
    print(use(c, 10, 20))
