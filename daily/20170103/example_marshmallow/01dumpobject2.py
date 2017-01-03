# -*- coding:utf-8 -*-
"""speed up"""
from marshmallow import Schema, fields, post_load


class PersonSchema(Schema):
    name = fields.String(required=True)
    age = fields.Integer(required=True)

    def get_attribute(self, k, ob, default):
        return getattr(ob, k, default)

    @post_load
    def lift(self, data):
        return Person(**data)


class Person(object):
    def __init__(self, name, age):
        self.name = name
        self.age = age

print(PersonSchema().dump(Person("foo", 20), update_fields=False))
# MarshalResult(data={'name': 'foo', 'age': 20}, errors={})
print(PersonSchema().load({'age': 20, 'name': 'foo'}))
