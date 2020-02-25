import dataclasses


@dataclasses.dataclass
class Person:
    name: str
    age: int
    # father: t.Optional["Person"]
    # mother: t.Optional["Person"]


@dataclasses.dataclass
class WPerson(Person):
    nickname: str = ""


foo = Person(name="foo", age=20)
# print(vars(foo))
# print(set(dir(foo)).difference(dir(object)))
# print(set(dir(foo.__class__)).difference(dir(object)))
for name, val in getattr(foo, dataclasses._FIELDS).items():
    print("@", name, "@", getattr(foo, name), "@", val)
print()

foo = WPerson(name="Foo", age=20, nickname="F")
for name, val in getattr(foo, dataclasses._FIELDS).items():
    print("@", name, "@", getattr(foo, name), "@", val)

print()
