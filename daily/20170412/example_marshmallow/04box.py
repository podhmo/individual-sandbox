
from swagger_marshmallow_codegen.schema import AdditionalPropertiesSchema
from marshmallow import fields


class Box(AdditionalPropertiesSchema):
    name = fields.String()

    class Meta:
        additional_field = fields.Integer()


print(Box().load({"name": "foo", "x": "100", "y": "200"}))
# UnmarshalResult(data={'x': 100, 'name': 'foo', 'y': 200}, errors={})
print(Box().load({"name": "foo", "x": "ababa", "y": "200"}))
# UnmarshalResult(data={'name': 'foo', 'y': 200}, errors={'x': ['Not a valid integer.']})
