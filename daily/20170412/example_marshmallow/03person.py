
from marshmallow import Schema, fields


class Person(Schema):
    name = fields.String(required=True)
    age = fields.Integer()

    # class Meta:
    #     additional = ("nickname",)
print(Person().load({"name": "foo", "nickname": "foo"}))
