import json
import jsonschema

with open("./schema.json") as rf:
    schema = json.load(rf)


data = {"name": "foo", "age": 10, "memo": "hai"}
print(jsonschema.validate(schema, data))
print(data)
# {'name': 'foo', 'age': 10, 'memo': 'hai'}
