from marshmallow import Schema, fields, post_load, pre_load


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
