from marshmallow import Schema, validate, fields


class S(Schema):
    v0 = fields.Int(validate=[validate.Range(min=1, max=10)])
    v1 = fields.Int(validate=[validate.Range(min=1, max=10, min_inclusive=False)])
    v2 = fields.Int(validate=[validate.Range(min=1, max=10, max_inclusive=False)])
    v3 = fields.Int(
        validate=[
            validate.Range(min=1, max=10, min_inclusive=False, max_inclusive=False)
        ]
    )

S().load({"v0": 10, "v1": 10, "v2": 10, "v3": 1})
# marshmallow.exceptions.ValidationError: {
# 'v2': ['Must be greater than or equal to 1 and less than 10.'], 
# 'v3': ['Must be greater than 1 and less than 10.']}


S().load({"v0": 1, "v1": 1, "v2": 1, "v3": 1})
# marshmallow.exceptions.ValidationError: {
# 'v3': ['Must be greater than 1 and less than 10.'],
# 'v1': ['Must be greater than 1 and less than or equal to 10.']}
