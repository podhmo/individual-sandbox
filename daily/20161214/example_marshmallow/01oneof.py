from marshmallow import Schema, fields, validate


class Foo(Schema):
    rgb = fields.String(validate=validate.OneOf(choices=["R", "G", "B"]))

print(Foo().load({"rgb": "R"}))
# UnmarshalResult(data={'rgb': 'R'}, errors={})

print(Foo().load({"rgb": "X"}))
# UnmarshalResult(data={}, errors={'rgb': ['Not a valid choice.']})
