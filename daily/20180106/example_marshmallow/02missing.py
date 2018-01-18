from marshmallow import Schema, fields


class S(Schema):
    v0 = fields.Int(missing=0)
    v1 = fields.Int(allow_none=True)
    v2 = fields.Int(missing=lambda: 0, allow_none=True)


print(S().load({}))
print(S().load({"v0": None, "v1": None, "v2": None}))
