# this is auto-generated by swagger-marshmallow-codegen
from __future__ import annotations
from marshmallow import (
    Schema,
    fields,
)


class Person(Schema):
    name = fields.String()
    x_data = fields.Dict(keys=fields.String(), values=fields.String(), data_key='x-data')