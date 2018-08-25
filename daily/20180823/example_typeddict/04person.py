import mypy_extensions as mx


class Person(mx.TypedDict, total=True):
    name: str
    age: int


class HasNickname(mx.TypedDict, total=False):
    nickname: str


class PersonHasNickname(Person, HasNickname):
    pass


# ok
d0: PersonHasNickname = {"name": "foo", "age": 20, "nickname": "F"}

# ok
d1: PersonHasNickname = {"name": "foo", "age": 20}

# error: Key 'age' missing for TypedDict "PersonHasNickname"
d2: PersonHasNickname = {"name": "foo", "nickname": "Fq"}
