import bson
from collections import ChainMap
import bson.json_util as u
import json

person = ChainMap({"name": "foo"}, ChainMap({"age": 20}, {"_id": bson.ObjectId()}))

try:
    print(json.dumps(person))
except Exception as e:
    print("hmm", e)

print("----------------------------------------")

print(json.dumps(person, indent=2, ensure_ascii=False, default=str))
print("----------------------------------------")


def default(d):
    if hasattr(d, "keys"):
        return dict(d)
    else:
        return str(d)


print(json.dumps(person, indent=2, ensure_ascii=False, default=default))

print("----------------------------------------")
print(u.dumps(person, indent=2, ensure_ascii=False))
