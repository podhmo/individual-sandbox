import typing as t

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

    def open(
        self, *, opener: t.Optional[t.Callable[[], U]] = None
    ) -> t.ContextManager[t.Union[T, U]]:
        if opener is None:
            return Wrapper(self.opener())

        return Wrapper(opener())


class Wrapper:
    def __init__(self, ob: T) -> None:
        self.ob = ob

    def __enter__(self) -> T:
        return self.ob

    @t.no_type_check
    def __exit__(self, x, y, z) -> None:
        pass


fs = FS(opener=Foo)

with fs.open() as foo:
    foo.foo()
with fs.open(opener=Foo) as foo:
    foo.foo()
with fs.open(opener=Bar) as bar:
    bar.bar()
