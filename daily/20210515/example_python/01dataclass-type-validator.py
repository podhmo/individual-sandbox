from __future__ import annotations
import typing as t
import dataclasses
from dataclass_type_validator import dataclass_validate
import json


@dataclass_validate
@dataclasses.dataclass
class Person:
    name: str
    age: int
    father: t.Optinal[Person] = None


s = """
{"name": "foo", "age": 20, "father": {"name": "bar", "age": 40}}
"""
d = json.loads(s)  # nestした表現がダメ
print(Person(**d))

s = """
{"name": "foo"}
"""
d = json.loads(s)
print(Person(**d))
