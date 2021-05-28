from __future__ import annotations
import functools
import typing as t
import dataclasses

T = t.TypeVar("T")


@functools.lru_cache(maxsize=None)
def get_undefined(name: str) -> Undefined:
    return Undefined(name)


class Undefined:
    def __init__(self, name: str) -> None:
        self.name = name

    def __repr__(self) -> str:
        return f"<undefined of {self.name}>"


class Prop(t.Generic[T]):
    def __init__(self, *, name: t.Optional[str] = None) -> None:
        self.name = name
        self.store_name = "_"

    def __repr__(self) -> str:
        cls = self.__class__
        return f"<{cls.__module__}.{cls.__name__} object at {hex(id(self))}, field={self.name!r}>"

    @t.overload
    def __get__(self, ob: None, typ: t.Optional[t.Type[t.Any]] = ...) -> Undefined:
        ...

    @t.overload
    def __get__(  # noqa:F811
        self, ob: Prop, typ: t.Optional[t.Type[t.Any]] = ...
    ) -> Prop[T]:
        ...

    def __get__(  # noqa:F811
        self, ob: t.Optional[t.Any], typ: t.Optional[t.Type[t.Any]] = None
    ) -> t.Union[T, Undefined]:
        if ob is None:
            return self

        if hasattr(ob, self.store_name):
            return getattr(ob, self.store_name)
        return get_undefined(self.name)

    def __set__(self, ob: t.Any, value: T) -> None:
        setattr(ob, self.store_name, value)

    def __set_name__(self, ob: t.Any, name: str) -> None:
        self.name = name
        self.store_name = f"_{name}"


class A:
    name = Prop[str]()


@dataclasses.dataclass
class B:
    name: str = Prop[str]()


print(A.name)
a = A()
print(a.name)
a.name = "foo"
print(a.name)

print("----------------------------------------")
print(B.name)
print(B())
print(B(name="foo"))
# dataclassesで消えちゃうからPropになっちゃうのか
