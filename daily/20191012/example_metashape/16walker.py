import typing as t

T = t.TypeVar("T")
F = t.TypeVar("F", bound=t.Callable[..., t.Any])
MetaData = t.Optional[t.Dict[str, t.Any]]


class Field(t.Generic[F]):
    wrapped: F
    metadata: t.Optional[MetaData]

    def __init__(self, wrapped: F, *, metadata: t.Optional[MetaData], cached):
        self.wrapped = wrapped
        self.metadata = metadata
        self.cached = cached
        try:
            self.__doc__ = wrapped.__doc__
        except:  # noqa
            pass

    def __get__(self, obj, type=None) -> T:
        if obj is None:
            return self.wrapped(None)  # xxx:

        val = self.wrapped(obj)
        if self.cached:
            setattr(obj, self.wrapped.__name__, val)
        return val


def field(
    *, metadata: t.Optional[MetaData] = None, cached: bool = False
) -> t.Callable[[F], Field[F]]:
    def decorated(wrapped: F):
        return Field(wrapped, metadata=metadata, cached=cached)

    return t.cast(t.Callable[[F], Field[F]], decorated)  # xxx: HACK


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


class WPerson(Person):
    @field(metadata=dict(description="hmm"))
    def name(self) -> str:
        """name docstring"""
        return "<name>"

    @field(metadata=dict(description="hmm"))
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
