from jsonschema import validate
from dictknife import loading
import contextlib

schema = loading.loadfile("./swagger.yaml")
schema["type"] = "object"
schema["oneOf"] = [
    {
        "$ref": "#/definitions/Person"
    },
    {
        "$ref": "#/definitions/Group"
    },
]


@contextlib.contextmanager
def check(msg):
    print("----------------------------------------")
    print(msg)
    print("----------------------------------------")
    try:
        yield
        print("ok")
    except Exception as e:
        print(e)


d = {"name": "foo"}
with check(d):
    validate(d, schema)

d = {"name": "foo", "age": 10}
with check(d):
    validate(d, schema)

d = {"name": "A", "members": []}
with check(d):
    validate(d, schema)

d = {"name": "B", "members": [{"name": "foo", "age": 10}]}
with check(d):
    validate(d, schema)

d = {"name": "C", "members": [{"name": "foo", "age": 10}], "age": 0}
with check(d):
    validate(d, schema)
