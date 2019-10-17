import typing as t

T = t.TypeVar("T")
F = t.TypeVar("F", bound=t.Callable[..., t.Any])


class Person:
    name: str
    age: int
    nickname: t.Optional[str] = None


def field(*, default: T, metadata: t.Dict[str, t.Any]) -> T:
    return t.cast(T, {"metadata": metadata, "default": default})  # xxx:HACK


def mark(f: F) -> F:
    def deco(*args, **kwargs):
        return f(*args, **kwargs)

    decorated = t.cast(F, deco)
    decorated.marked = True  # type: ignore
    decorated.__annotations__ = f.__annotations__
    return decorated


class Person2:
    name: str
    nickname: t.Optional[str] = field(default=None, metadata=dict(description="hello"))


class Person3:
    @mark
    def name(self, default="") -> str:
        return default

    @mark
    def nickname(self, default=None) -> t.Optional[str]:
        return default


class Person4(Person3):
    @mark
    def nickname(self, default="") -> str:
        return default


_cache: t.Dict[str, t.Type[t.Any]] = {}


def walk(
    typ: t.Type[t.Any], *, _cache=_cache
) -> t.Iterable[t.Tuple[str, t.Type[t.Any]]]:
    for fieldname, fieldtype in t.get_type_hints(typ).items():
        yield fieldname, fieldtype

    if hasattr(typ, "mro"):
        # cache
        marked_cache = _cache.get(typ)
        if marked_cache is not None:
            for fieldname, fieldtype in marked_cache.items():
                yield fieldname, fieldtype
        else:
            marked_cache = _cache[typ] = {}
            for cls in typ.mro():
                for fieldname, fieldtype in cls.__dict__.items():
                    if getattr(fieldtype, "marked", None) is None:
                        continue
                    if fieldname in marked_cache:
                        continue
                    fieldtype = fieldtype.__annotations__.get("return")
                    marked_cache[fieldname] = fieldtype
                    yield fieldname, fieldtype


for x in walk(Person):
    print(x)
print("----------------------------------------")
for x in walk(Person2):
    print(x)
print("----------------------------------------")
for x in walk(Person3):
    print(x)
print("----------------------------------------")
for x in walk(Person3):
    print(x)
print("----------------------------------------")
for x in walk(Person4):
    print(x)
print("========================================")
print(Person2.nickname)
