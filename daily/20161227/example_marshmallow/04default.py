from marshmallow import Schema, fields


class S(Schema):
    nums = fields.List(fields.Integer(), missing=lambda: [1, 2, 3], default=lambda: [-1, -2, -3])

print(S().load({}))
# UnmarshalResult(data={'nums': [1, 2, 3]}, errors={})
print(S().dump({}))
# MarshalResult(data={'nums': [-1, -2, -3]}, errors={})
