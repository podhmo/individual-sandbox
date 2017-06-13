from marshmallow import Schema, fields


class S(Schema):
    n = fields.Integer()


try:
    S(strict=True).load({"n": "foo"})
except Exception as e:
    print("error", e.data)
