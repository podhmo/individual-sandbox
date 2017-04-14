from jsonschema import Draft4Validator


schema = {
    "type": ["string", "null"]
}

validate = Draft4Validator(schema).validate
print(validate("foo", schema))
print(validate(None, schema))
print(validate(None, {"type": "string"}))
