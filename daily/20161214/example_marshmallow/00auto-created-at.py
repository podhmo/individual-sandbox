from marshmallow import Schema, fields
from datetime import datetime


class Foo(Schema):
    created_at = fields.DateTime(default=datetime.now)

print("load:", Foo().load({}).data)
print("dump:", Foo().dump({}).data)

# load: {}
# dump: {'created_at': datetime.datetime(2016, 12, 14, 12, 58, 10, 789947)}
