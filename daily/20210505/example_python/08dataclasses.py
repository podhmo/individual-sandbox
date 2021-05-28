import typing as t
import dataclasses


class Base:
    type_: t.ClassVar[str] = ""
    _registered: t.ClassVar[t.Dict[str, t.Type[t.Any]]] = {}

    def __init_subclass__(cls, *, type_: t.Optional[str] = None):
        if type_ is None:
            type_ = cls.__name__

        if type_ in cls._registered:
            assert cls._registered[type_] == cls
        cls._registered[type_] = cls

        cls.type_ = type_
        cls.__annotations__["type_"] = str  # xxx:


@dataclasses.dataclass
class A(Base):
    name: str


@dataclasses.dataclass
class AA(A, type_="XA"):
    name: str


@dataclasses.dataclass
class B(Base):
    name: str


print(A(name="foo"))
print(AA(name="foo"))
print(A._registered)

a = A(name="foo")
print(a.name)
print(a.type_)
