# -*- coding:utf-8 -*-
# this is auto-generated by swagger-marshmallow-codegen
from marshmallow import (
    Schema,
    fields,
)


class Api(Schema):
    secret_key = fields.String(required=True, dump_to='secret-key', load_from='secret-key')


class Db(Schema):
    host = fields.String(required=True)
    name = fields.String(required=True)
    port = fields.Integer()


class Flags(Schema):
    testing = fields.Boolean(required=True)


class Config(Schema):
    api = fields.Nested('Api')
    db = fields.Nested('Db')
    flags = fields.Nested('Flags')
