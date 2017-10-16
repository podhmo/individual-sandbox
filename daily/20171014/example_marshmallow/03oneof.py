"""
definitions:
  Target:
    oneOf:
      - {$ref: "#/definitions/Person"}
      - {$ref: "#/definitions/Group"}
"""

from swagger_marshmallow_codegen.schema import OneOfSchema
from schema import Person, Group


class Target(OneOfSchema):
    schema_classes = (Person, Group)


print(Target().load({"name": "foo"}))
t = Target()
print(t.load({"name": "foo"}))
print(t.load({"name": "foo", "age": 10}))
print(t.load([{"name": "foo", "age": 10}], many=True))
print(t.load([{"name": "foo", "age": 10}, {"name": "foo"}], many=True))

print("----------------------------------------")
print(Target().load({"name": "foo", "members": []}))
print("----------------------------------------")
print(t.dump({"name": "foo", "age": 10}))
print(t.dump({"name": "foo", "members": []}))
print(t.dump({"name": "foo", "age": 10, "members": []}))
print("----------------------------------------")
