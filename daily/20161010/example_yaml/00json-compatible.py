import yaml
import json
from io import StringIO
from jsonschema import validate

schema = {
    "type": "object",
    "patternProperties": {
        "\d{3}": {"type": "string"}
    },
    "additionalProperties": False
}

# error
# validate({200: "ok"}, schema)

# ok
validate(json.loads(json.dumps({200: "ok"})), schema)


# but
def loads(s):
    io = StringIO(s)
    return yaml.load(io)


def dumps(d):
    io = StringIO()
    yaml.dump(d, io)
    return io.getvalue()

# ng
# validate(loads(dumps({200: "ok"})), schema)

print(loads("!!str 200: ok"))
print(loads("'200': ok"))


def construct_json_compatible_map(loader, node):
    return {str(k): v for k, v in loader.construct_pairs(node)}
yaml.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, construct_json_compatible_map)


validate(loads(dumps({200: "ok"})), schema)
