# -*- coding:utf-8 -*-
from marshmallow import Schema, fields


class PersonSchema(Schema):
    name = fields.String(required=True)
    age = fields.Integer(required=True)


class Person(object):
    def __init__(self, name, age):
        self.name = name
        self.age = age

print(PersonSchema().dump(Person("foo", 20)))
# MarshalResult(data={'name': 'foo', 'age': 20}, errors={})
