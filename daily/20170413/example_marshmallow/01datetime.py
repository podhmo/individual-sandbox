from marshmallow import Schema
from swagger_marshmallow_codegen.fields import Date


class S(Schema):
    v = Date(required=True, allow_none=True)

print(S().load({"v": None}))
print(S().load({"v": "200-01-01"}))
print(S().load({}))
