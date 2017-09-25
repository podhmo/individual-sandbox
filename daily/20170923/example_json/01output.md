``` python
import datetime as dt
import extjson


@extjson.register(dt.datetime)
def encode_datetime(o):
    return o.isoformat()


d = {
    "name": "foo",
    "birth": dt.datetime.now(),
}
print(extjson.dumps(d))

# {
#   "birth": "2017-09-24T12:58:36.471604",
#   "name": "foo"
# }
```
