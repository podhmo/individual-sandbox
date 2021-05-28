# from __future__ import annotations
import typing as t
import dataclasses
import marshmallow_dataclass
import json


@dataclasses.dataclass
class Person:
    name: str
    age: int
    father: t.Optional["Person"] = None


PersonSchema = marshmallow_dataclass.class_schema(Person)

s = """
{"name": "foo", "age": 20, "father": {"name": "bar", "age": 40}}
"""
d = json.loads(s)
print(PersonSchema().load(d))

s = """
{"name": "foo"}
"""
d = json.loads(s)
print(PersonSchema().load(d))
