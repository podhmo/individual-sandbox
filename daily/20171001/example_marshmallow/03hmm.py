from marshmallow import Schema, fields, validate


class S(Schema):
    v = fields.Integer(validate=validate.Range(max=10))


print(S().load({"v": 1000000000000000}))
