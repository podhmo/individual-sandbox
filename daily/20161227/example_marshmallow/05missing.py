from marshmallow import Schema, fields
import uuid
from datetime import datetime


class S(Schema):
    d = fields.DateTime(missing=datetime.now)


print(S().load({}))
# UnmarshalResult(data={}, errors={'d': ['Not a valid datetime.']})


class S1(Schema):
    d = fields.DateTime(missing=lambda: str(datetime.now()))


print(S1().load({}))
# UnmarshalResult(data={'d': datetime.datetime(2017, 1, 7, 1, 14, 58, 474789)}, errors={})


class S2(Schema):
    d = fields.UUID(missing=uuid.uuid4)

print(S2().load({}))
