from marshmallow import Schema, fields


class S(Schema):
    v = fields.Integer(required=True)


print(S().load({}))
s = S()
s.fields["v"].required = False
print(s.load({}))
print(S().load({}))
