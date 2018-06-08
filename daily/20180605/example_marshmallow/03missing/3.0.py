import sys
import datetime
from marshmallow import (
    Schema,
    fields,
)


class Default(Schema):
    string = fields.String(missing=lambda: 'default')
    integer = fields.Integer(missing=lambda: 10)
    boolean = fields.Boolean(missing=lambda: True)
    date = fields.Date(missing=lambda: datetime.date(2000, 1, 1))
    datetime = fields.DateTime(missing=lambda: datetime.datetime(2000, 1, 1, 1, 1, 1))


if __name__ == "__main__":
    try:
        d = {}
        data = Default().load(d)
        print("ok", data)
        print(Default().dump(data))
    except Exception as e:
        print("ng", e)
        sys.exit(-1)

# ok {'date': datetime.date(2000, 1, 1), 'integer': 10, 'datetime': datetime.datetime(2000, 1, 1, 1, 1, 1), 'boolean': True, 'string': 'default'}
# {'date': '2000-01-01', 'integer': 10, 'datetime': '2000-01-01T01:01:01+00:00', 'boolean': True, 'string': 'default'}
