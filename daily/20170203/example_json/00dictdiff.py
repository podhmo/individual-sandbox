import json

person = {"name": "foo", "age": 10}
print(json.dumps(person, indent=2, ensure_ascii=False, sort_keys=True))
