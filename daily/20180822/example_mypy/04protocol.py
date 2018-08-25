import typing as t
import typing_extensions as tx


class P(tx.Protocol):
    def f(self, **kwargs: t.Any) -> None:
        ...


class A:
    def f(self, verbose: bool = False) -> None:
        pass


def use(p: P) -> None:
    p.f()


def main():
    a = A()
    use(a)
