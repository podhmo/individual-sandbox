# -*- coding:utf-8 -*-
import jsonschema
from collections import ChainMap


schema = {
    "type": "object",
    "properties": {
        "name": {"type": "string"}
    }
}

data = {"name": "foo", "age": 20}

print(jsonschema.validate(data, schema))

schema2 = ChainMap(schema, {"additionalProperties": False})
try:
    print(jsonschema.validate(data, schema2))
except:
    print("error schema2")
