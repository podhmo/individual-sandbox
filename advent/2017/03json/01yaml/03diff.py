import yaml
import json

d = {
    "/": {
        200: {
            "description": "ok response"
        },
        "default": {
            "description": "default response"
        },
    },
}

with open("schema.yaml", "w") as wf:
    yaml.dump(d, wf)
with open("schema.yaml", "r") as rf:
    from_yaml = yaml.load(rf)

with open("schema.json", "w") as wf:
    json.dump(d, wf)
with open("schema.json", "r") as rf:
    from_json = json.load(rf)

assert from_yaml == from_json  # AssertionError
