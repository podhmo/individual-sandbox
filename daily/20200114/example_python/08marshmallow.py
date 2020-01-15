import dataclasses
from marshmallow import Schema, fields, post_load


@dataclasses.dataclass
class Person:
    name: str
    age: int


class PersonSchema(Schema):
    name = fields.String(required=True)
    age = fields.Integer()

    @post_load
    def to_dataclass(self, d, **kwargs):
        return Person(**d)


print(PersonSchema().load({"name": "foo", "age": 10}))
