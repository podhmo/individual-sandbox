from marshmallow import Schema, fields


class S(Schema):
    v = fields.Integer(required=True, allow_none=True)

print(S().load({"v": None}))
print(S().load({"v": "10"}))
print(S().load({}))
