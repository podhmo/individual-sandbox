from marshmallow import Schema, fields


class A(Schema):
    name = fields.String(missing="a")


class B(Schema):
    name = fields.String(missing="b")
    a = fields.Nested(A, missing=A().load({}))


b = B().load({})
print(b)
