from marshmallow import Schema, fields
from marshmallow.validate import Regexp


class S(Schema):
    email = fields.String(validate=Regexp(".+@.+"))


class S2(Schema):
    d = fields.Date()

data = [{"email": "a@b.jp"}, {"email": "xy"}, {"email": "a@b.jp"}]
print(S(many=True).load(data))

print(S2().dump({"d": "1"}))
print(S2().load({"d": "2011-01-01"}))
# MarshalResult(data={}, errors={'d': ['"1" cannot be formatted as a date.']})
