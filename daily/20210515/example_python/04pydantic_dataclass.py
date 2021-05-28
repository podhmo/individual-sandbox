from __future__ import annotations
import typing as t
import pydantic.dataclasses as dataclasses
import json


@dataclasses.dataclass
class Person:
    name: str
    age: int
    father: t.Optional[Person] = None


Person.__pydantic_model__.update_forward_refs()

s = """
{"name": "foo", "age": 20, "father": {"name": "bar", "age": 40}}
"""
d = json.loads(s)
print(Person(**d))

s = """
{"name": "foo"}
"""
d = json.loads(s)
print(Person(**d))
