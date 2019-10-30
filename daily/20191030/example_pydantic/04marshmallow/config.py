import marshmallow
from marshmallow import fields
from marshmallow import validate

"""
    port:      uint16
    logLevel:  "debug" | *"info" | "warn" | "error" | "critical"
    secondary: <self>
    xxxAPI:
      token: string
"""


class Config(marshmallow.Schema):
    port = fields.Integer(required=True)
    logLevel = fields.String(
        required=True,
        validate=[validate.OneOf(["debug", "info", "warn", "error", "critical"])],
    )
    secondary = fields.Nested("Config")
    xxxAPI = fields.Nested("XXXAPI")


class XXXAPI(marshmallow.Schema):
    token = fields.String(required=True)
