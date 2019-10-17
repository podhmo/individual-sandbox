import typing as t

T = t.TypeVar("T")
MetaData = t.Optional[t.Dict[str, t.Any]]


class Field(t.Generic[T]):
    default: T
    metadata: t.Optional[MetaData]

    def __init__(self, default: T, *, metadata: t.Optional[MetaData] = None):
        self.default = default
        self.metadata = metadata

    def __get__(self, obj, type=None) -> T:
        return self.default


def field(*, default: T, metadata: t.Dict[str, t.Any] = None) -> T:
    return t.cast(T, Field(default, metadata=metadata))  # xxx: HACK


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
    age: int = field(default=0)


class WPerson(Person):
    nickname: t.Optional[str] = field(default=None, metadata=dict(description="hmm"))


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
print(wp.nickname)
wp.nickname = "foo"
print(wp.nickname)
