import json

s = """
{
    "name": "foo",
    "age": 20,
    "name": "bar"
}
"""
print(json.loads(s))  # {'name': 'bar', 'age': 20}
