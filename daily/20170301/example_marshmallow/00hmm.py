# https://github.com/marshmallow-code/marshmallow/issues/588
from datetime import datetime
from marshmallow import Schema, fields
from marshmallow.utils import get_value, missing


class Foo(object):
    def __init__(self, created_at=None):  # or created_at=missing
        self.created_at = created_at


FOO_TIME_FORMAT = '%Y%m%d%H%M%S'


class FooSchema(Schema):
    created_at = fields.LocalDateTime(
        format=FOO_TIME_FORMAT,
        default=lambda: datetime(2017, 5, 4, 3, 2, 1),
        missing=lambda: datetime(2017, 9, 8, 7, 6, 5).strftime(FOO_TIME_FORMAT),
        required=False, allow_none=False,
    )

    @classmethod
    def get_attribute(self, attr, obj, default):
        return get_value(attr, obj, default=default) or missing


schema = FooSchema()
print(schema.load({}))
# UnmarshalResult(data={'created_at': datetime.datetime(2017, 9, 8, 7, 6, 5)}, errors={})
print(schema.loads('{}'))
print(schema.dump({}))
# MarshalResult(data={'created_at': datetime.datetime(2017, 5, 4, 3, 2, 1)}, errors={})
print(schema.dump(Foo()))
# MarshalResult(data={'created_at': None}, errors={})
