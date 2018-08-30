import json


s = "{\"name\": \"foo\", \"age\": 20}"
print(json.dumps(json.loads(s)))
