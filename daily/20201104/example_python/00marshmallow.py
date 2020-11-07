from __future__ import annotations
import typing as t
import dataclasses
from marshmallow import Schema, fields, post_load


@dataclasses.dataclass
class User:
    name: str
    age: int


class UserSchema(Schema):
    name = fields.String(required=True)
    age = fields.Integer(missing=lambda: 0)

    @post_load
    def make_object(self, data: t.Dict, **kwargs: t.Dict[str, t.Any]) -> User:
        ob = User(**data)
        return ob


s = UserSchema()
print(s.load({"name": "foo"}))

s = UserSchema(many=True)
print(s.load([{"name": "foo"}, {"name": "bar"}]))
