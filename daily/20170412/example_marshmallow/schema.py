# -*- coding:utf-8 -*-
from marshmallow import (
    Schema,
    fields
)


class Box(Schema):
    name = fields.String()
