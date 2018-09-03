import mypy_extensions as mx


class PersonOptional(mx.TypedDict, total=False):
    nickname: str


class Person(PersonOptional, total=True):
    name: str
    age: int


def main() -> None:
    d: Person = {"name": "foo", "age": 20}
    d2: Person = {"name": "foo", "age": 20, "nickname": "x"}
    # d3: Person = {"name": "foo", "nickname": "x"} # error: Key 'age' missing for TypedDict "Person"
    # d4: Person = {}  # error: Keys ('name', 'age') missing for TypedDict "Person"
    # d5: Person = {"name": "foo", "age": 20, "nick": "x"}  #  Extra key 'nick' for TypedDict "Person"
    p(d)


def p(d: Person) -> None:
    print(d)
