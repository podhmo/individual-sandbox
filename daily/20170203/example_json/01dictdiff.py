import json
import datetime

person = {"name": "foo", "age": 10, "createdAt": datetime.date(2000, 1, 1)}
print(json.dumps(person, indent=2, ensure_ascii=False, sort_keys=True, default=str))
