from __future__ import annotations
import typesystem
import json

definitions = typesystem.SchemaDefinitions()


class Person(typesystem.Schema, definitions=definitions):
    name = typesystem.String()
    age = typesystem.Integer()
    father = typesystem.Reference(to="Person", allow_null=True)


s = """
{"name": "foo", "age": 20, "father": {"name": "bar", "age": 40}}
"""
d = json.loads(s)
print(Person.validate(d))

s = """
{"name": "foo"}
"""
try:
    d = typesystem.validate_json(s, Person)
    print(d)
except typesystem.ValidationError as e:
    for message in e.messages():
        line_no = message.start_position.line_no
        column_no = message.start_position.column_no
        print(f"  Error {message.text!r} at line {line_no}, column {column_no}.")
