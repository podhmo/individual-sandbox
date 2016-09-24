# -*- coding:utf-8 -*-
import jsonschema
import json
import logging
logging.basicConfig(level=logging.DEBUG)

with open("swagger-2.0.json") as rf:
    schema = json.load(rf)

with open("valid-swagger.json") as rf:
    data = json.load(rf)
    data.pop("swagger", None)

print(jsonschema.validate(data, schema))
