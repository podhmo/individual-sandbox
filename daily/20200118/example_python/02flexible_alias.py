import typing as t
import mypy_extensions as mx

Foo = mx.FlexibleAlias[str, int]
Bar = mx.FlexibleAlias[str, int]


def foo(foo: Foo) -> None:
    print(foo)
    print(foo.upper())
    print(foo.__str__())

bar = t.cast(Bar, "bar")
print(bar)
foo(bar)
