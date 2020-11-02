from marshmallow import Schema, fields


class Sin(Schema):
    N = fields.Float(required=True)
