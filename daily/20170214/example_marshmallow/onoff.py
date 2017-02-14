from marshmallow import Schema, fields


class MyBoolean(fields.Boolean):
    truthy = fields.Boolean.truthy.union({"on"})
    falsy = fields.Boolean.falsy.union({"off"})


class S(Schema):
    v = MyBoolean()


print(S().load({"v": True}))
print(S().load({"v": False}))
print(S().load({"v": "on"}))
print(S().load({"v": "off"}))

# UnmarshalResult(data={'v': True}, errors={})
# UnmarshalResult(data={'v': False}, errors={})
# UnmarshalResult(data={'v': True}, errors={})
# UnmarshalResult(data={'v': False}, errors={})
