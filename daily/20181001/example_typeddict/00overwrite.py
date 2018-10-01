import mypy_extensions as mx


class PersonOptional(mx.TypedDict, total=False):
    name: str
    age: int


class Person(PersonOptional, total=True):
    name: str


d0: Person = {"name": "foo", "age": 20}  # ok
d1: Person = {"name": "foo"}  # ok
d2: Person = {}  # ng
d3: Person = {"name": "foo", "age": "20"}  # ng
