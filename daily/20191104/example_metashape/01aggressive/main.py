from __future__ import annotations
import typing as t
from metashape import runtime


class Person:
    name: str
    age: int
    father: t.Optional[Person]
    mother: t.Optional[Person]


w = runtime.get_walker(Person, aggressive=True)
assert [Person] == list(w.walk())
