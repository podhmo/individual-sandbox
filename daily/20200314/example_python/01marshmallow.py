import typing
import json
from marshmallow import fields, Schema, INCLUDE


class Person(Schema):
    class Meta:
        unknown = INCLUDE

    name = fields.String(required=True)
    age = fields.Integer(required=True)

    def dump(self, obj: typing.Any, *, many: bool = None):
        # do serialization if needed in extra fields.
        d = obj.copy()

        d.update(super().dump(obj, many=many))
        return d


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
