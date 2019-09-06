from swagger_marshmallow_codegen.schema import PrimitiveValueSchema
from marshmallow import fields, Schema


class S(PrimitiveValueSchema):
    class schema_class(Schema):
        value = fields.Integer(required=True)

S()
