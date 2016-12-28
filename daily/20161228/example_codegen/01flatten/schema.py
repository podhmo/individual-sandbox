# -*- coding:utf-8 -*-
from marshmallow import(
    Schema,
    fields
)
from marshmallow.validate import OneOf


class Person(Schema):
    id = fields.String(required=True)
    name = fields.String(required=True)
    age = fields.Integer()
    skills = fields.List(fields.Nested('PersonSkillsItem', ))
    relations = fields.List(fields.Nested('PersonRelationsItem', ))


class PersonRelationsItem(Schema):
    direction = fields.String(validate=[OneOf(choices=['following', 'followed', 'bidirectional'], labels=[])])
    personId = fields.String()


class PersonSkillsItem(Schema):
    name = fields.String()
