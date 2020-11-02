from marshmallow import Schema, fields


class S(Schema):
    name = fields.String(required=True)
    age = fields.Int()


print((s := S()).load({"name": "foo"}))
