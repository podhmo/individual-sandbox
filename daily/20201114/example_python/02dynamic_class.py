from __future__ import annotations
import typing as t


class Person:
    name: str
    age: t.Optional[int]


print(Person, t.get_type_hints(Person))
DynamicallyGeneratedPerson = type(
    "Person", (), {"__annotations__": {"name": str, "age": t.Optional[int]}}
)
print(DynamicallyGeneratedPerson, t.get_type_hints(DynamicallyGeneratedPerson))
