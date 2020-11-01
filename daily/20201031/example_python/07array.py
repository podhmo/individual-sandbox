from __future__ import annotations
import typing as t


class Person:
    name: str
    age: int
    children: list[Person]
    children2: t.List[Person]

print(t.get_type_hints(Person))
