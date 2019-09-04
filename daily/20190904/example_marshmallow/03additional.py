from swagger_marshmallow_codegen.schema import AdditionalPropertiesSchema
from marshmallow import fields


class S(AdditionalPropertiesSchema):
    name = fields.String()


S().load({"name": "foo", "value": 10})
