from marshmallow import Schema, fields
from marshmallow.utils import missing as missing_


class State(Schema):
    status = fields.String()
    priority = fields.Int()
    memo = fields.String(allow_none=True, required=True, missing=None)

class S(Schema):
    state = fields.Nested(State, attribute="*me*")

print(S().load({"state": {"status": "error", "priority": 1}}))
# UnmarshalResult(data={'*me*': {'priority': 1, 'status': 'error'}}, errors={})

