import jsonschema

spec = {
    "definitions": {
        "person": {
            "type": "object",
            "properties": {
                "name": {
                    "type": "string"
                },
                "age": {
                    "type": "integer",
                },
            },
            "required": ["name", "age"],
        },
    },
    "allOf": [
        {
            "$ref": "#/definitions/person"
        },
        {
            "required": ["name"],
        },
    ],
}


# print(jsonschema.validate({}, spec))
print(jsonschema.validate({"name": "foo"}, spec))
