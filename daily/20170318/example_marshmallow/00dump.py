from marshmallow import Schema, fields
from datetime import datetime


class S(Schema):
    dt = fields.DateTime()

s = S()
data = {"dt": datetime.now()}
print(s.validate(data))

data = {"dt": '2017-03-19T01:50:15.872393+00:00'}
print(s.validate(data))
