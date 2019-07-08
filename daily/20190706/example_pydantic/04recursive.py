from __future__ import annotations
import typing as t
from pydantic import BaseModel


class Person(BaseModel):
    name: str
    age: int
    father: Person = None
    mother: Person = None


Person.update_forward_refs()
person = Person.parse_obj(
    {"name": "foo", "age": 20, "father": {"name": "boo", "age": 20}}
)
print(person)
print(person.dict())
