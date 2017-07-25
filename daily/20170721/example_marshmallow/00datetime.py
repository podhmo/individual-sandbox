from marshmallow import Schema, fields

# deserialize: str[TZ] -> datetime[TZ]
# serialize: datetime[TZ] -> str[UTC]


class S(Schema):
    d = fields.DateTime()


d = {"d": "2017-06-27T07:49:40-04:00"}
s = S(strict=True)
print(s.load(d).data)
# {'d': datetime.datetime(2017, 6, 27, 7, 49, 40, tzinfo=tzoffset(None, -14400))}
print(s.dump(s.load(d).data).data)
