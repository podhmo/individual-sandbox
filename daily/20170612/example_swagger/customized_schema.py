from marshmallow import fields
from swagger_marshmallow_codegen.schema import PrimitiveValueSchema
from marshmallow.validate import Regexp


class Emails(PrimitiveValueSchema):
    v = fields.String(validate=[Regexp('.+@.+')])
