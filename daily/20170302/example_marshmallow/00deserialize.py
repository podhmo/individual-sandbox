from marshmallow import Schema, fields


class S0(Schema):
    num = fields.Integer(missing="1")


class S1(Schema):
    num = fields.Integer(missing=1)

print(S0().load({}))
print(S1().load({}))
# UnmarshalResult(data={'num': 1}, errors={})
# UnmarshalResult(data={'num': 1}, errors={})
