from marshmallow import Schema, fields


class S(Schema):
    v = fields.Int()

s = S()
print("load", s.load({"v": "10"}))
print("validate", s.validate({"v": 10}))
print("validate(ng)", s.validate({"v": "10"}))
print("dump", s.dump({"v": "10"}))
