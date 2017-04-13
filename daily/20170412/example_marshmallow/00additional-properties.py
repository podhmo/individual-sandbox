from marshmallow import Schema, fields, ValidationError
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

class Box(Schema):
    name = fields.String()
    _additional_field = fields.Integer()

    @pre_load
    def pre_load(self, data):
        diff = set(data.keys()).difference(self.fields.keys())
        for name in diff:
            self.fields[name] = fields.Integer()
        return data

    @pre_dump
    def pre_dump(self, data):
        diff = set(data.keys()).difference(self.fields.keys())
        for name in diff:
            self.fields[name] = fields.Integer()
        return data

    def dumps(self, obj, many=None, update_fields=False, *args, **kwargs):
        return super().dumps(obj, many=many, update_fields=update_fields, *args, **kwargs)

    def dump(self, obj, many=None, update_fields=False, *args, **kwargs):
        return super().dump(obj, many=many, update_fields=update_fields, *args, **kwargs)

print(Box().load({"name": "foo", "value": 100, "v": "a"}))
print(Box().dump({"name": "foo", "value": 100}))
print(Box().dump({"name": "foo", "value": 100, "v": "a"}))
