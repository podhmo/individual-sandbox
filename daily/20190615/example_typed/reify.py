import typing as t

T = t.TypeVar("T")


class reify:
    def __init__(self, wrapped: t.Callable[[t.Any], T]) -> None:
        self.wrapped = wrapped
        try:
            self.__doc__ = wrapped.__doc__
        except:  # noqa
            pass

    def __get__(self, inst: t.Any, objtype: t.Optional[t.Any] = None) -> T:
        if inst is None:
            return self  # type: ignore
        val = self.wrapped(inst)
        setattr(inst, self.wrapped.__name__, val)
        return val


class A:
    @reify
    def value(self) -> int:
        return 10

    @property
    def value2(self) -> int:
        return 10


# broken
print(reveal_type(A().value))
print(A().value.replace("foo", "bar"))
print(A().value.bit_length())
print(reveal_type(A().value2))
