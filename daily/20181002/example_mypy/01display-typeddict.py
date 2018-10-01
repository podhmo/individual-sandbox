import typing as t
import mypy_extensions as mx


class Person(mx.TypedDict):
    name: str
    age: int


class PersonWithNickname(Person, total=False):
    nickname: str


pd: Person = {"name": "foo", "age": 20}
print(t.get_type_hints(Person))  # {'name': <class 'str'>, 'age': <class 'int'>}
print(vars(Person))
print(vars(PersonWithNickname))
