from marshmallow import Schema, fields
from marshmallow import validates_schema, ValidationError


class S(Schema):
    name = fields.String(required=True)
    v = fields.Integer(required=True)

    class Meta:
        ordered = True


print(S().load({"v": "10"}))
print(S().load({"name": "foo"}))
print(S().load({"name": "foo", "v": 10}))


class S2(S):
    @validates_schema
    def validate_(self, data):
        raise ValidationError("hmm")


print(S2().load({"name": "foo", "v": 10}))
print(S2(many=True).load([{"name": "foo", "v": 10}]))
