import typing as t

T = t.TypeVar("T")


def metadata(key: str, value: t.Any) -> t.Callable[[T], T]:
    def set(prop: T) -> T:
        setattr(prop, key, value)
        return prop

    return set


class Person:
    name: str

    def __init__(self, name: str) -> None:
        self.name = name


class Person2:
    def __init__(self, name: str) -> None:
        self._name = name

    @property
    @metadata("ja", "名前")
    def name(self) -> str:
        return self._name


print(t.get_type_hints(Person))
print(t.get_type_hints(Person2), Person2.__annotations__)
# hmm
