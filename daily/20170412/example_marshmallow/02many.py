from datetime import datetime
from swagger_marshmallow_codegen.schema import AdditionalPropertiesSchema
from marshmallow import fields


class Box(AdditionalPropertiesSchema):
    class Meta:
        additional_field = fields.DateTime()


print(Box().load({"d": "2017-01-01T00:00:00Z"}))
print(Box().dump({"d": datetime.now()}))
print(Box(many=True).load([{"d": "2017-01-01T00:00:00Z"}, {"d2": "2017-01-01T00:00:00Z"}]))
print(Box(many=True).dump([{"d": datetime.now()}, {"d2": datetime.now()}]))
