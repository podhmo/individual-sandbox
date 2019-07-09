import typing as t
from pydantic import BaseModel
from fastapi.encoders import jsonable_encoder


class Person(BaseModel):
    name: str
    age: int
    parents: t.List["Person"] = []


Person.update_forward_refs()


obj = Person.parse_obj(
    {"name": "foo", "age": 20, "parents": [{"name": "boo", "age": 40}]}
)
print(jsonable_encoder(obj))
print(obj.dict())
