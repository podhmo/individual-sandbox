``` python
from marshmallow import Schema, fields


class S(Schema):
    v = fields.Integer(required=True)


print(S().load({"v": 10}))
print(S().load({"v": 10, "extra": "extra"}))

print("----------------------------------------")

s = S()
s.fields["extra"] = fields.String(required=True)
print(s.load({"v": 10}))
print(s.load({"v": 10, "extra": "extra"}))

# UnmarshalResult(data={'v': 10}, errors={})
# UnmarshalResult(data={'v': 10}, errors={})
# ----------------------------------------
# UnmarshalResult(data={'v': 10}, errors={'extra': ['Missing data for required field.']})
# UnmarshalResult(data={'v': 10, 'extra': 'extra'}, errors={})
```
