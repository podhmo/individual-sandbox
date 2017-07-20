import json
from minicollections import valueobject

s = """
{
  "name": "foo",
  "age": 20
}
"""

Person = valueobject("Person", [("name", str, ""), ("age", int, 0)])


def on_object(d):
    return Person(**d)


print(json.loads(s, object_hook=on_object))
