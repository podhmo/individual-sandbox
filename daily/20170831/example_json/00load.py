import json

s = """
{"name": "foo"}
{"name": "bar"}
{"name": "boo"}
"""
print(json.loads(s))
