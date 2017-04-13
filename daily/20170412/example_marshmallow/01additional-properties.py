from marshmallow import Schema, fields, SchemaOpts
from marshmallow import pre_load, pre_dump
# simulate additional properties

"""
{
  "type": "object",
  "additionalProperties": {
    "type": "integer"
  },
  "properties": {
    "name": {"type": "string"}
  }
}
"""


class AdditionalPropertiesOpts(SchemaOpts):
    def __init__(self, meta, **kwargs):
        super().__init__(meta, **kwargs)
        self.additional_field = getattr(meta, "additional_field", fields.Field)


class AdditionalPropertiesSchema(Schema):
    """
    support addtionalProperties

    class MySchema(AdditionalPropertiesSchema):
        class Meta:
            additional
    """

    OPTIONS_CLASS = AdditionalPropertiesOpts

    @pre_load
    def pre_load(self, data):
        diff = set(data.keys()).difference(self.fields.keys())
        for name in diff:
            self.fields[name] = self.opts.additional_field()
        return data

    @pre_dump
    def pre_dump(self, data):
        diff = set(data.keys()).difference(self.fields.keys())
        for name in diff:
            self.fields[name] = self.opts.additional_field()
        return data

    def dumps(self, obj, many=None, update_fields=False, *args, **kwargs):
        return super().dumps(obj, many=many, update_fields=update_fields, *args, **kwargs)

    def dump(self, obj, many=None, update_fields=False, *args, **kwargs):
        return super().dump(obj, many=many, update_fields=update_fields, *args, **kwargs)


class Box(AdditionalPropertiesSchema):
    name = fields.String()

    class Meta:
        additional_field = fields.Integer


class Box2(AdditionalPropertiesSchema):
    name = fields.String()


b = Box()
print(b.load({"name": "foo", "value": "100"}))
print(b.load({"name": "foo", "value": "100", "v": "a"}))
print(b.dump({"name": "foo", "value": "100"}))
print(b.dump({"name": "foo", "value": "100", "v": "a"}))
b = Box2()
print(b.load({"name": "foo", "value": "100"}))
print(b.load({"name": "foo", "value": "100", "v": "a"}))
print(b.dump({"name": "foo", "value": "100"}))
print(b.dump({"name": "foo", "value": "100", "v": "a"}))
