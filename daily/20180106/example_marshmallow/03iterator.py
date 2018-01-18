from marshmallow import Schema, fields


class S0(Schema):
    name = fields.String(required=True)


class S(Schema):
    def get_marshalling_iterator(self, additional_field=fields.Int()):
        def iterate_with_additional_field(data, fields_dict):
            for k in data:
                field = fields_dict.get(k, additional_field)
                yield k, field

        return iterate_with_additional_field

    name = fields.String(required=True)


print(S0().load({"name": "foo", "a": "10", "b": 0}))
print(S().load({"name": "foo", "a": "10", "b": 0}))
print("----------------------------------------")
print(S0().dump({"name": "foo", "a": 10}))
print(S().dump({"name": "foo", "a": 10}))
