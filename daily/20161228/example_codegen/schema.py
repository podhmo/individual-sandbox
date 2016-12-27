# -*- coding:utf-8 -*-
from marshmallow import(
    Schema,
    fields
)
from marshmallow.validate import(
    Length
)
from swagger_marshmallow_codegen.fields import DateTime


class Pet(Schema):
    id = fields.String(description='Unique identifier')
    name = fields.String(required=True, description="Pet's name", validate=[Length(min=1, max=100, equal=None)])
    animal_type = fields.String(required=True, description='Kind of animal', validate=[Length(min=1, max=None, equal=None)])
    tags = fields.Field(description='Custom tags')
    created = DateTime(description='Creation time')
