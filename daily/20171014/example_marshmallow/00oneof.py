"""
definitions:
  Target:
    oneOf:
      - {$ref: "#/definitions/Person"}
      - {$ref: "#/definitions/Group"}
"""

from marshmallow import Schema, fields
from swagger_marshmallow_codegen.schema import ComposedSchema, OneOfStrategy
from schema import Person, Group


class Target(ComposedSchema):
    strategy_class = OneOfStrategy
    schema_classes = (Person, Group)


print(Target().load({"name": "foo"}))
print(Target().load({"name": "foo", "age": 10}))
print(Target().load({"name": "A", "members": []}))
print(Target().load({"name": "B", "members": [{"name": "foo", "age": 10}]}))
print(Target().load({"name": "B", "age": 10, "members": [{"name": "foo", "age": 10}]}))
print("-")
print(Target(many=True).load([{"name": "foo"}]))
print(Target(many=True).load([{"name": "foo", "age": 10}]))
print(Target(many=True).load([{"name": "A", "members": []}]))
print(Target(many=True).load([{"name": "B", "members": [{"name": "foo", "age": 10}]}]))
print(Target(many=True).load([{"name": "B", "age": 10, "members": [{"name": "foo", "age": 10}]}]))
print(Target(many=True).load([{"name": "A", "age": 10}, {"name": "B"}, {"name": "C", "age": 10}]))
print("-")
print(Person(many=True).load([{"name": "A", "age": 10}, {"name": "A"}, {"name": "B", "age": 10}]))


class S(Schema):
    target = fields.Nested(Target)


print(S().load({"target": {}}))
print(S().load({"target": {"name": "foo", "age": 20}}))
print("-")
print(Target(many=True).load({}))
print(Person(many=True).load({}))
