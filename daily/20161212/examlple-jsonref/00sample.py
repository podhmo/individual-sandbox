# -*- coding:utf-8 -*-
from pprint import pprint
import jsonref

# An example json document
json_str = """{"real": [1, 2, 3, 4], "ref": {"$ref": "#/real"}}"""
data = jsonref.loads(json_str)
pprint(data)  # Reference is not evaluated until here
# {'real': [1, 2, 3, 4], 'ref': [1, 2, 3, 4]}
