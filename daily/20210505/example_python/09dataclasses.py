import typing as t
import dataclasses
import enum


class Kind(enum.Enum):
    unknown = enum.auto()
    A = enum.auto()
    XA = enum.auto()
    B = enum.auto()


class Base:
    type_: t.ClassVar[Kind] = Kind.unknown
    _registered: t.ClassVar[t.Dict[str, t.Type[t.Any]]] = {}

    def __init_subclass__(cls, *, kind: t.Optional[str] = None):
        if kind is None:
            kind = cls.__name__

        kind_value = getattr(Kind, kind)

        if kind_value in cls._registered:
            registered = cls._registered[kind_value]
            assert registered == cls, (cls, registered)
        cls._registered[kind_value] = cls

        cls.kind = kind_value
        cls.__annotations__["kind"] = Kind  # xxx:


@dataclasses.dataclass
class A(Base):
    name: str


@dataclasses.dataclass
class AA(A, kind="XA"):
    name: str


@dataclasses.dataclass
class B(Base):
    name: str


print(A(name="foo"))
print(AA(name="foo"))
print(A._registered)

a = A(name="foo")
print(a.name)
print(a.kind)
