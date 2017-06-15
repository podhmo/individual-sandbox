from marshmallow import Schema, fields
from marshmallow import MarshalResult, UnmarshalResult


class PrimitiveValueSchema:
    schema_class = None
    key = "value"
    missing = None

    def __init__(self, *args, **kwargs):
        self.schema = self.__class__.schema_class(*args, **kwargs)

    def load(self, value):  # don't support many
        data = {self.key: value}
        r, errors = self.schema.load(data)
        return UnmarshalResult(
            data=r.get(self.key) or self.missing,
            errors=errors.get(self.key) or errors,
        )

    def dump(self, value):  # don't support many
        data = {self.key: value}
        r, errors = self.schema.dump(data, update_fields=False)
        return MarshalResult(
            data=r.get(self.key) or self.missing,
            errors=errors.get(self.key) or errors,
        )


class IntSchema(PrimitiveValueSchema):
    class schema_class(Schema):
        value = fields.Integer(required=True)


class IntListSchema(PrimitiveValueSchema):
    key = "values"

    class schema_class(Schema):
        values = fields.List(fields.Integer(), required=True)


print(IntSchema().load(10))
print(IntSchema().dump(10))
print(IntSchema().load("foo"))
print(IntSchema().dump("foo"))
print(IntListSchema().load([10, 20]))
print(IntListSchema().dump(["foo", "bar"]))
