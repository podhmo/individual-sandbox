import pydantic
import typing_extensions as tx

"""
    port:      uint16
    logLevel:  "debug" | *"info" | "warn" | "error" | "critical"
"""


class Config(pydantic.BaseModel):
    port: int
    logLevel: tx.Literal["debug", "info", "warn", "error", "critical"]
