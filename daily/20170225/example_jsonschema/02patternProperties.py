import jsonschema

spec = {
    "type": "object",
    "properties": {
        "name": {"type": "string"}
    },
    "patternProperties": {
        ".+Score$": {
            "type": "integer"
        },
    },
    "additionalProperties": False,
}

print(jsonschema.validate({"name": "foo", "mathScore": 100, "japaneseScore": 80}, spec))
