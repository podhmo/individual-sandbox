from __future__ import annotations
import typing as t
from metashape.declarative import shape
from metashape import runtime


@shape
class Person:
    name: str
    age: int
    father: t.Optional[Person]
    mother: t.Optional[Person]


w = runtime.get_walker(Person)
assert [Person] == list(w.walk())
