import typesystem

"""
    port:      uint16
    logLevel:  "debug" | *"info" | "warn" | "error" | "critical"
"""


class Config(typesystem.Schema):
    port = typesystem.Integer()
    logLevel = typesystem.Choice(
        title="LogLevel",
        choices=[(x, x) for x in ["debug", "info", "warn", "error", "critical"]],
    )
