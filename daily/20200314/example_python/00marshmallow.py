import json
from marshmallow import fields
from swagger_marshmallow_codegen.schema import AdditionalPropertiesSchema


class Person(AdditionalPropertiesSchema):
    name = fields.String(required=True)
    age = fields.Integer(required=True)


s = Person()
code = """\
{"name": "foo", "age": 20, "nickname": "F"}
"""

print("input:")
print(code)
data = s.load(json.loads(code))

print("got:")
print(data)

print()
print("output")
print(json.dumps(s.dump(data)))
