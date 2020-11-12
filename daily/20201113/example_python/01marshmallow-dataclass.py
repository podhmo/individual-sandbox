# from __future__ import annotations
# not supported..

import typing as t
import dataclasses
from marshmallow import Schema
from marshmallow_dataclass import dataclass


@dataclass(frozen=True)
class Person:
    name: str
    age: t.Optional[int] = None
    father: t.Optional["Person"] = None
    mother: t.Optional["Person"] = None
    Schema: t.ClassVar[t.Type[Schema]] = Schema  # For the type checker


def as_dict(schema: Schema, ob: dataclasses.dataclass) -> t.Dict[str, t.Any]:
    d = schema.omit_none(schema.from_dataclass(ob))
    d = schema.load(d)  # or validate()?
    return schema.dump(d)


data = {"name": "foo"}
print(Person.Schema().load(data))
data = {"name": "foo", "father": {"name": "boo", "age": "20"}}
print(Person.Schema().load(data))
print(dataclasses.asdict(Person.Schema().load(data)))
print("-")
print(Person.Schema().dump(Person(name="foo")))
print(Person.Schema().dump({"name": "foo"}))
print("-")
print(Person.Schema().validate({"name": "foo", "age": "xxx"}))
print("-")
