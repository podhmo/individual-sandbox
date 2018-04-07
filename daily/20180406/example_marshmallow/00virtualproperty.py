from marshmallow import Schema
from marshmallow import fields
from marshmallow import post_load


class AdSchema(Schema):
    """Schema Mapping class."""

    id = fields.Int()
    available_formats = fields.List(fields.Str(), dump_only=True)

    @post_load
    def make_ad(self, data):
        return AdModel(**data)


class AdModel:
    def __init__(self, id):
        self.id = id

    @property
    def available_formats(self):
        return ["x", "y", "z"]


d = {"id": 10, "available_formats": ["a", "b"]}
print(AdSchema().load(d))
