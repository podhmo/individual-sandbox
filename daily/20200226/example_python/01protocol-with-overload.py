import typing as t
import typing_extensions as tx


class P(tx.Protocol):
    @t.overload
    def foo(self, x: str) -> None:
        ...

    @t.overload
    def foo(self, x: str, *args: t.Any) -> None:
        ...

    def foo(self, x: str, *args: t.Any) -> None:
        ...


class Foo:
    def foo(self, x: t.Any, *args: t.Any) -> None:
        if args:
            print(x, args)
        else:
            print(x)


class Boo:
    def foo(self, x: t.Any) -> None:
        print(x)


def use(p: P) -> None:
    p.foo("xxx")
    p.foo("xxx", "yyy")


use(Foo())
use(Boo())
