from __future__ import annotations
import typing as t
import pydantic
import json


class Person(pydantic.BaseModel):
    name: str
    age: int
    father: t.Optional[Person] = None


Person.update_forward_refs()

s = """
{"name": "foo", "age": 20, "father": {"name": "bar", "age": 40}}
"""
print(Person.parse_raw(s))

s = """
{"name": "foo"}
"""
d = json.loads(s)
try:
    print(Person(**d))
except pydantic.ValidationError as err:
    print(err.json())
    raise
