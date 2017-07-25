from marshmallow import Schema, fields
import pytz


# deserialize: str[TZ] -> datetime[UTC]
# serialize: datetime[UTC] -> str[UTC]

class ForceUTCDateTime(fields.DateTime):
    def _deserialize(self, value, attr, data):
        dt = super()._deserialize(value, attr, data)
        return dt.astimezone(pytz.utc)


class S(Schema):
    d = ForceUTCDateTime()


d = {"d": "2017-06-27T07:49:40-04:00"}
s = S(strict=True)
print(s.load(d).data)
# {'d': datetime.datetime(2017, 6, 27, 11, 49, 40, tzinfo=<UTC>)}
print(s.dump(s.load(d).data).data)

d = {"d": "2017-01-01T09:00:00+09:00"}
s = S(strict=True)
print(s.load(d).data)
# {'d': datetime.datetime(2017, 1, 1, 0, 0, tzinfo=<UTC>)}
print(s.dump(s.load(d).data).data)
