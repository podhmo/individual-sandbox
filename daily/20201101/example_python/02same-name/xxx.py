from marshmallow import Schema, fields


class Sin(Schema):
    name = fields.String(required=True)
