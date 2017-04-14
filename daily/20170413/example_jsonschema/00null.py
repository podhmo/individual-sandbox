from jsonschema import validate
schema = {
    "type": ["string", "null"]
}
print(validate("foo", schema))
print(validate(None, schema))
print(validate(None, {"type": "string"}))
