import typing_extensions as tx


class Person(tx.TypedDict):
    name: str
    age: int


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
