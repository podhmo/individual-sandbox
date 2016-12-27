from marshmallow import Schema, fields


class S(Schema):
    n = fields.Integer(missing=1, default=-1)

print(S().load({}))
# UnmarshalResult(data={'n': 1}, errors={})
print(S().dump({}))
# MarshalResult(data={'n': -1}, errors={})
