# -*- coding:utf-8 -*-
from marshmallow import (
    Schema,
    fields
)


class Person(Schema):
    """original (no. 1)"""
    name = fields.String()


class Mix(Schema):
    """no. 2"""
    age = fields.Integer()


class Person2(Person, Mix):
    pass


class Person3(Person):
    """no. 3"""
    age = fields.Integer()


class Person4(Person):
    """no. 4"""
    age = fields.Integer()


class Person5(Schema):
    """no. 5"""
    name = fields.String(required=True)
    age = fields.Integer()
