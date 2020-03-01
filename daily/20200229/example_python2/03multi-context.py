import typing as t
import contextlib

T = t.TypeVar("T")
U = t.TypeVar("U")


class Foo:
    def foo(self) -> None:
        print("foo")


class Bar:
    def bar(self) -> None:
        print("bar")


class FS(t.Generic[T]):
    def __init__(self, opener: t.Callable[[], T]) -> None:
        self.opener = opener

    @t.overload
    def open(self) -> t.ContextManager[T]:
        ...

    @t.overload
    def open(self, *, opener: t.Callable[[], U]) -> t.ContextManager[U]:
        ...

    # error: Overloaded function implementation cannot produce return type of signature 1
    # error: Overloaded function implementation cannot produce return type of signature 2
    @contextlib.contextmanager
    def open(
        self, *, opener: t.Optional[t.Callable[[], U]] = None
    ) -> t.Union[t.Iterator[t.Union[T, U]]]:
        if opener is None:
            yield self.opener()
        else:
            yield opener()


fs = FS(opener=Foo)

with fs.open() as foo:
    foo.foo()
with fs.open(opener=Foo) as foo:
    foo.foo()
with fs.open(opener=Bar) as bar:
    bar.bar()
