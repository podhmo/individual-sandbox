from marshmallow import fields, Schema, INCLUDE


class S(Schema):
    class Meta:
        unknown = INCLUDE

    name = fields.String()


print(S().load({"name": "foo", "value": 10}))
