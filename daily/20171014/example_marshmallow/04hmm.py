from marshmallow import Schema, fields, post_load, pre_load, validates_schema, ValidationError


class S(Schema):
    @pre_load
    def lowercase(self, data):
        return {k.lower(): v for k, v in data.items()}

    @post_load
    def wrap(self, data):
        return {"wrap": data}

    name = fields.String(required=True)
    age = fields.Integer(required=True)


print(S().load({"NAME": "foo"}))
print(S().load({"NAME": "foo", "AGE": "10"}))
print(S(many=True).load([{"NAME": "foo", "AGE": "10"}, {"NAME": "foo", "AGE": "10"}]))


class S2(S):
    @validates_schema
    def hmm(self, data):
        raise ValidationError("hmm")


class S3(Schema):
    s = fields.Nested(S2, required=True)


print(S2().load({}))
