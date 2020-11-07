from __future__ import annotations
import typing as t
from marshmallow import Schema, fields, post_load
import o


class UserSchema(Schema):
    name = fields.String(required=True)
    age = fields.Integer(missing=lambda: 0)

    @post_load
    def make_object(self, data: t.Dict, **kwargs: t.Dict[str, t.Any]) -> o.User:
        ob = o.User(**data)
        return ob
