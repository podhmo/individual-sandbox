# -*- coding:utf-8 -*-
# this is auto-generated by swagger-marshmallow-codegen
from marshmallow import (
    Schema,
    fields
)


class Person(Schema):
    name = fields.String(required=True, attribute='name')
    age = fields.Integer(required=True, attribute='age')
    nickName = fields.String(attribute='nick_name')
