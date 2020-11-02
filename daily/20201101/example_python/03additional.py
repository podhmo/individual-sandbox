from marshmallow import Schema, fields, INCLUDE


class S(Schema):
    class Meta:
        unknown = INCLUDE

    name = fields.String(required=True)
    age = fields.Int()


print((s := S()).load({"name": "foo", "x-xxx": "xxx"}))
