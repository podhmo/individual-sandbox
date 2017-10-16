"""
definitions:
  Target:
    oneOf:
      - {$ref: "#/definitions/Person"}
      - {$ref: "#/definitions/Group"}
"""

from marshmallow import fields, Schema
from swagger_marshmallow_codegen.schema import ComposedSchema, OneOfStrategy
from schema import Person, Group


class Target(ComposedSchema):
    strategy = OneOfStrategy()
    schema_classes = (Person, Group)


class S(Schema):
    target = fields.Nested(Target)


t = S()
print(t.dump({"target": {"name": "foo", "age": 10}}))
print(t.dump({"target": {"name": "foo", "age": 10, "members": []}}))
