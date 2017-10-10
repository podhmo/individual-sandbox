from marshmallow import Schema, fields
from marshmallow_polyfield import PolyField


class Person(Schema):
    name = fields.String(required=True)
    age = fields.Integer(required=True)


class Group(Schema):
    name = fields.String(required=True)
    members = fields.List(fields.Nested(Person()), required=True)


def selector_for_deserialize(d, parent):
    if parent.get("type") == "group":
        return Group()
    else:
        return Person()


def selector_for_serialize(ob, parent):
    if "members" in ob:
        parent["type"] = "group"
        return Group()
    else:
        parent["type"] = "person"
        return Person()


class S(Schema):
    type = fields.String(required=True)
    ob = PolyField(
        serialization_schema_selector=selector_for_serialize,
        deserialization_schema_selector=selector_for_deserialize,
        required=True
    )


print(S().load({"ob": {"name": "foo", "age": 20}, "type": "person"}))
print(S().load({"ob": {"name": "A", "members": [{"name": "foo", "age": 20}]}, "type": "group"}))
print(S().dump({"ob": {"name": "foo", "age": 20}}))
print(S().dump({"ob": {"name": "A", "members": [{"name": "foo", "age": 20}]}}))
