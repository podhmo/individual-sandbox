# -*- coding:utf-8 -*-
from jsonschemaerror import check_json
import json
import logging
logging.basicConfig(level=logging.DEBUG)

with open("swagger-2.0.json") as rf:
    schema = json.load(rf)

with open("valid-swagger.json") as rf:
    data = json.load(rf)
    data["paths"]["/products"]["get"]["responses"]["200"].pop("description", None)
    data.pop("swagger", None)
    data.pop("info", None)

print(check_json(data, schema))
