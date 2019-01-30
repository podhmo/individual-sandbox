import json
import jsonpatch

# https://github.com/evanphx/json-patch/issues/69
doc = {
    "env": [
        {
            "name": "weight",
            "value": "121"
        },
        {
            "name": "height",
            "value": "160"
        },
        {
            "name": "age"
        },
        {
            "name": "job",
            "value": "teacher"
        },
    ]
}
patches = [
    {
        "op": "move",
        "from": "/env/3",
        "path": "/env/1"
    },
]

result = jsonpatch.JsonPatch(patches).apply(doc)
print(json.dumps(result, indent=2, sort_keys=True))
{
    "env": [
        {
            "name": "weight",
            "value": "121"
        }, {
            "name": "job",
            "value": "teacher"
        }, {
            "name": "height",
            "value": "160"
        }, {
            "name": "age"
        }
    ]
}
