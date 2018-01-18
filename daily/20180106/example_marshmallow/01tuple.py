from marshmallow import Schema, fields, validate


class Tuple(fields.List):
    def _serialize(self, value, attr, obj):
        if value is None:
            return None
        return tuple(super()._serialize(value, attr, obj))

    def _deserialize(self, value, attr, data):
        return tuple(super()._deserialize(value, attr, data))


class S(Schema):
    nums = Tuple(fields.Int(), validate=[validate.Length(min=1)])


print(S().load({}))
print(S().load({"nums": []}))
print(S().load({"nums": [1]}))
