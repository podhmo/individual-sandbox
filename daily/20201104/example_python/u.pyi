import typing as t
from marshmallow import Schema
import o


class UserSchema(Schema):
    def load(self, data: t.Dict[str, t.Any]) -> o.User:
        ...
