import typing_extensions as tx
import typing as t


class Person(tx.TypedDict):
    name: str
    age: int


# print(vars(Person))
print(t.get_type_hints(Person).keys())
