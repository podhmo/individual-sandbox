from __future__ import annotations
import typing as t
import json
from pydantic import BaseModel
from pydantic.schema import schema


class Person(BaseModel):
    name: str
    age: int
    data: t.Dict[str, str]
    nested_data: t.Dict[str, t.Dict[str, str]]
    count: t.Dict[int, int]

toplevel_schema = schema([Person])
print(json.dumps(toplevel_schema, indent=2, ensure_ascii=False))
