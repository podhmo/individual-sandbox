import typing as t

T = t.TypeVar("T")
F = t.TypeVar("F", bound=t.Callable[..., t.Any])


def f(x: T) -> T:
    return x


def wrap(fn: F) -> F:
    def deco(*args, **kwargs):
        print("hoi")
        return fn(*args, **kwargs)

    return t.cast(F, deco)


def wrap_property(fn: F) -> F:  # decorated property is not supported
    def deco(*args, **kwargs):
        print("hoi")
        return fn(*args, **kwargs)

    return property(t.cast(F, deco))


class X:
    @property
    def name(self) -> str:
        return "<name>"

    @property
    @wrap
    def name2(self) -> str:
        return "<name2>"

    @wrap_property
    def name3(self) -> str:
        return "<name3>"


if t.TYPE_CHECKING:
    reveal_type(f("x"))
    reveal_type(f(1))
    reveal_type(property(lambda x: "x"))

    reveal_type(X.name)
    reveal_type(X().name)

    reveal_type(X.name2)
    reveal_type(X().name2)

    reveal_type(X.name3)
    reveal_type(X().name3)

print("@", X.name2, "-", X().name2)
print("@", X.name3, "-", X().name3)
