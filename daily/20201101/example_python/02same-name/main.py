from marshmallow import Schema, fields
import xxx
import yyy


class S(Schema):
    name = fields.String(required=True)
    xxx = fields.Nested(lambda: xxx.Sin, required=True)
    yyy = fields.Nested(lambda: yyy.Sin, required=True)


print((s := S()).load({"name": "foo", "xxx": {"name": "X"}, "yyy": {"N": 1.0}}))
