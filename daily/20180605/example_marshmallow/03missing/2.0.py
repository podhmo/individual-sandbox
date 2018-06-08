import sys
import datetime
from swagger_marshmallow_codegen.fields import Date, DateTime
from marshmallow import (
    Schema,
    fields,
)


class Default(Schema):
    string = fields.String(missing=lambda: 'default')
    integer = fields.Integer(missing=lambda: 10)
    boolean = fields.Boolean(missing=lambda: True)
    date = Date(missing=lambda: datetime.date(2000, 1, 1))
    datetime = DateTime(missing=lambda: datetime.datetime(2000, 1, 1, 1, 1, 1))


if __name__ == "__main__":
    try:
        d = {}
        data = Default().load(d)
        print("ok", data)
        print(Default().dump(data))
    except Exception as e:
        print("ng", e)
        sys.exit(-1)

# ok UnmarshalResult(data={'boolean': True, 'string': 'default', 'date': datetime.date(2000, 1, 1), 'integer': 10, 'datetime': datetime.datetime(2000, 1, 1, 1, 1, 1)}, errors={})
# MarshalResult(data={}, errors={})
