from jsonschema.validators import Draft4Validator, extend
from jsonschema._validators import type_draft4


def type_custom(validator, types, instance, schema, nullable_attr="x-nullable"):
    if schema.get(nullable_attr, False):
        if not isinstance(types, (list, tuple)):
            types = [types]
        types.append("null")
    yield from type_draft4(validator, types, instance, schema)


CustomValidator = extend(Draft4Validator, {
    "type": type_custom,
})

schema = {
    "type": ["string", "null"]
}

validate = CustomValidator(schema).validate
print(validate(None, schema))
print(validate(None, {"type": "string", "x-nullable": True}))
