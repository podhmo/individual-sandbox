import jsonschema

spec = {
    "type": "object",
    "properties": {
        "name": {
            "type": ["string", "null"]
        },
    },
    "required": [
        "name"
    ]

}

# print(jsonschema.validate({}, spec))
print(jsonschema.validate({"name": None}, spec))
