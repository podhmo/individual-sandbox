from marshmallow import Schema, fields


class S(Schema):
    n = fields.Integer()


print(S().dump({"n": 1.0}))
print(S(many=True).dump([{"n": 1.0}]))
