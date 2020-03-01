import contextlib
import typing as t

T = t.TypeVar("T")


class Foo:
    def foo(self) -> None:
        print("foo")


class Bar:
    def bar(self) -> None:
        print("bar")


class FS:
    @contextlib.contextmanager
    def open(self, opener: t.Callable[[], T]) -> t.Iterator[T]:
        ob = opener()
        yield ob


fs = FS()

with fs.open(Foo) as foo:
    foo.foo()
with fs.open(Bar) as bar:
    bar.bar()
