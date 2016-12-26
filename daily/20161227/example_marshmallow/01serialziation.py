from marshmallow import fields, Schema


class S(Schema):
    nums = fields.Integer(many=True, default=[1, 2, 3])

print(S().dump({}).data)
# {'nums': [1, 2, 3]}
