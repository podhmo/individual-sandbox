import json
import datetime


def uppercase(d):
    if isinstance(d, dict):
        return {k.upper(): uppercase(v) for k, v in d.items()}
    elif isinstance(d, (list, tuple)):
        return [uppercase(x) for x in d]
    else:
        return d

person = {"name": "foo", "age": 10, "createdAt": datetime.date(2000, 1, 1)}
print(json.dumps(person, indent=2, ensure_ascii=False, sort_keys=True, default=str))
person2 = uppercase(person)
print(json.dumps(person2, indent=2, ensure_ascii=False, sort_keys=True, default=str))
