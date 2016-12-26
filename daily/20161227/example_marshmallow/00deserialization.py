from marshmallow import fields, Schema


class S(Schema):
    nums = fields.Integer(many=True, missing="[1, 2, 3]")

# this is error.
print(S().load({}))
# UnmarshalResult(data={}, errors={'nums': ['Not a valid integer.']})


# if missing's value is 1, error is not raised. but, this is not good, at least me.
class S2(Schema):
    nums = fields.Integer(many=True, missing="1")
print(S2().load({}))
# UnmarshalResult(data={'nums': 1}, errors={})
