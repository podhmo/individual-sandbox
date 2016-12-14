from marshmallow import Schema, fields
from datetime import datetime


class Foo(Schema):
    created_at = fields.DateTime(default=datetime.now)

print("----------------------------------------")
print("load:", Foo().load({}).data)
print("dump:", Foo().dump({}).data)

# load: {}
# dump: {'created_at': datetime.datetime(2016, 12, 14, 12, 58, 10, 789947)}

print("----------------------------------------")
d = Foo().load({"created_at": "2016-12-12T00:00:00Z"}).data
print("load:", d)
print("dump:", Foo().dump(d).data)
# load: {'created_at': datetime.datetime(2016, 12, 12, 0, 0, tzinfo=tzutc())}
# dump: {'created_at': '2016-12-12T00:00:00+00:00'}
