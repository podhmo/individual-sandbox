import marshmallow
from marshmallow import fields
from marshmallow import validate

"""
    port:      uint16
    logLevel:  "debug" | *"info" | "warn" | "error" | "critical"
"""


class Config(marshmallow.Schema):
    port = fields.Integer(required=True)
    logLevel = fields.String(
        required=True,
        validate=[validate.OneOf(["debug", "info", "warn", "error", "critical"])],
    )
