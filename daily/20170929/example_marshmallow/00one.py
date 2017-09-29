# -*- coding:utf-8 -*-
# https://github.com/marshmallow-code/marshmallow/issues/675
from marshmallow import Schema, fields


class UserSchema(Schema):
    first_attr = fields.Field(required=False)
    second_attr = fields.Field(required=False)


# need: mutual exclusive definition. (first_attr and second_attr)
