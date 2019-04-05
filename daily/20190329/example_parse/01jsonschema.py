import jsonschema
import json

schema_definition = {
    "type": "object",
    "properties": {"name": {"type": "string"}, "age": {"type": "integer"}},
}

jsonschema.Draft4Validator.check_schema(schema_definition)
schema = jsonschema.Draft4Validator(schema_definition)
with open("./data/ok.json") as rf:
    d = json.load(rf)
    print(d)
    for err in schema.iter_errors(d):
        print(err, vars(err))
