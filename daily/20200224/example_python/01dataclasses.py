import typing as t
import dataclasses


@dataclasses.dataclass
class Person:
    name: str
    age: int
    father: t.Optional["Person"] = None
    mother: t.Optional["Person"] = None


@dataclasses.dataclass
class WPerson(Person):
    nickname: str = ""


foo = Person(name="foo", age=20)

for name, field in getattr(foo, dataclasses._FIELDS).items():
    print("@", name, "@", getattr(foo, name))
print()

foo = WPerson(name="Foo", age=20, nickname="F")
for name, field in getattr(foo, dataclasses._FIELDS).items():
    print("@", name, "@", getattr(foo, name))

foo = WPerson(name="Foo", age=20, nickname="F", father=Person("boo", age=20))
for name, field in getattr(foo, dataclasses._FIELDS).items():
    print("@", name, "@", getattr(foo, name))

print()
