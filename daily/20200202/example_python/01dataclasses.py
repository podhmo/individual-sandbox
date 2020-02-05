import typing_extensions as tx
import typing as t
import dataclasses


@dataclasses.dataclass
class Person:
    name: str = "foo"
    age: int


# print(vars(Person))
print(t.get_type_hints(Person).keys())
