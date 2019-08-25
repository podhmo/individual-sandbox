import jsonschema

schema = {
    "type": "object",
    "properties": {"price": {"type": "number"}, "name": {"type": "string"}},
}

instance = {"name": "Eggs", "price": 34.99}
jsonschema.validate(instance=instance, schema=schema)

instance = {"name": "Eggs", "price": "invalid"}
try:
    jsonschema.validate(instance=instance, schema=schema)
except Exception as e:
    from pprint import pprint
    pprint(vars(e))
    raise

"""
{'cause': None,
 'context': [],
 'instance': 'invalid',
 'message': "'invalid' is not of type 'number'",
 'parent': None,
 'path': deque(['price']),
 'relative_path': deque(['price']),
 'relative_schema_path': deque(['properties', 'price', 'type']),
 'schema': {'type': 'number'},
 'schema_path': deque(['properties', 'price', 'type']),
 'validator': 'type',
 'validator_value': 'number'}
Traceback (most recent call last):
  File "16schema-validation.py", line 13, in <module>
    jsonschema.validate(instance=instance, schema=schema)
  File "VENV/lib/python3.7/site-packages/jsonschema/validators.py", line 899, in validate
    raise error
jsonschema.exceptions.ValidationError: 'invalid' is not of type 'number'

Failed validating 'type' in schema['properties']['price']:
    {'type': 'number'}

On instance['price']:
    'invalid'
"""
