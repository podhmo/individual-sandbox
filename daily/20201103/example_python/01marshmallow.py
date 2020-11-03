from marshmallow import Schema, fields


class S(Schema):
    name = fields.String(required=True)
    s2 = fields.Nested(lambda: S2(), required=True)


class S2(Schema):
    name = fields.String(required=True)


print((data := S().load({"name": "foo", "s2": {"name": "bar"}})))
