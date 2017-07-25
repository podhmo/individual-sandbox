from marshmallow import Schema, fields

# deserialize: str[TZ] -> datetime[TZ]
# serialize: datetime[TZ] -> str[TZ]


class S(Schema):
    d = fields.LocalDateTime()


d = {"d": "2017-06-27T07:49:40-04:00"}
s = S(strict=True)
print(s.load(d).data)
# {'d': datetime.datetime(2017, 6, 27, 7, 49, 40, tzinfo=tzoffset(None, -14400))}
print(s.dump(s.load(d).data).data)
# {'d': '2017-06-27T07:49:40-04:00'}

d = {"d": "2017-01-01T09:00:00+09:00"}
s = S(strict=True)
print(s.load(d).data)
# {'d': datetime.datetime(2017, 1, 1, 9, 0, tzinfo=tzoffset(None, 32400))}
print(s.dump(s.load(d).data).data)
# {'d': '2017-01-01T09:00:00+09:00'}
