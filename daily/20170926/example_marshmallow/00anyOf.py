from marshmallow import Schema, fields, ValidationError
from marshmallow.validate import Length, Range


class AnyOf(fields.Field):
    def __init__(self, subfields, *args, **kwargs):
        if not isinstance(subfields, (list, tuple)):
            subfields = [subfields]
        self.subfields = subfields
        super().__init__(*args, **kwargs)

    def _deserialize(self, value, attr, data):
        errors = []
        try:
            for subfield in self.subfields:
                v = subfield.deserialize(value, attr, data)
                return v
        except Exception as e:
            errors.append(e)
        raise errors[0]

    def _serialize(self, value, attr, obj):
        return value


class StrictString(fields.String):
    def _deserialize(self, value, attr, data):
        v = super()._deserialize(value, attr, data)
        if v.isdigit():
            raise self.fail(attr)
        return v


s = {"anyOf": [{"type": "string", "maxLength": 5}, {"type": "number", "minimum": 0}]}


class S(Schema):
    v = AnyOf([
        StrictString(validate=[Length(max=5)]),
        fields.Integer(validate=[Range(min=5)]),
    ])


print(S(strict=False).load({"v": "a"}))
print(S(strict=False).load({"v": "abcde"}))
print(S(strict=False).load({"v": "abcdefg"}))
print(S(strict=False).load({"v": "100"}))
print(S(strict=False).load({"v": "-1"}))
