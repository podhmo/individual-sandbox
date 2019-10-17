import typing as t

T = t.TypeVar("T")
F = t.TypeVar("F", bound=t.Callable[..., t.Any])
MetaData = t.Optional[t.Dict[str, t.Any]]


class Field(t.Generic[F]):
    wrapped: F

    def __init__(self, wrapped: F):
        self.wrapped = wrapped
        try:
            self.__doc__ = wrapped.__doc__
        except:  # noqa
            pass
        self.metadata = {"doc": getattr(self, "__doc__", None)}

    def __get__(self, obj, type=None) -> T:
        return self.wrapped(obj)


def get_metadata(cls: t.Type[t.Any], name: str) -> t.Optional[MetaData]:
    prop = cls.__dict__.get(name)
    if prop is None:
        return None
    return prop.metadata


def walk(
    typ: t.Type[t.Any]
) -> t.Iterable[t.Tuple[str, t.Type[t.Any], t.Optional[MetaData]]]:
    for fieldname, fieldtype in t.get_type_hints(typ).items():
        yield fieldname, fieldtype, get_metadata(typ, fieldname)


class Person:
    name: str
    age: int = 0


def field(fn: F) -> Field[F]:
    return Field(fn)


class WPerson(Person):
    @field
    def name(self) -> str:
        """name docstring"""
        return "<name>"

    @field
    def nickname(self) -> t.Optional[str]:
        """nickname docstring"""
        return None


print(WPerson.nickname, WPerson.age)
print(get_metadata(WPerson, "nickname"))
print("----------------------------------------")
for x in walk(WPerson):
    print(x)

if t.TYPE_CHECKING:
    reveal_type(WPerson.nickname)
    reveal_type(WPerson().nickname)


print("========================================")
wp = WPerson()
print(wp.name, wp.nickname)
wp.nickname = "foo"
print(wp.name, wp.nickname)
