import typing_extensions as tx


class PersonOptional(tx.TypedDict, total=False):
    age: int


class Person(PersonOptional, total=True):
    name: str


try:
    d = Person(name="foo")
    print("00", d)
except Exception as e:
    print("00", e)

try:
    d = Person({"name": "foo"})
    print("01", d)
except Exception as e:
    print("01", e)

try:
    d = Person({"age": 20})
    print("02", d)
except Exception as e:
    print("02", e)
