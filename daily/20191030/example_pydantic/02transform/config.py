import pydantic
import typing_extensions as tx

"""
    port:      uint16
    log_level:  "debug" | *"info" | "warn" | "error" | "critical"
"""


class Config(pydantic.BaseModel):
    port: int
    log_level: tx.Literal["debug", "info", "warn", "error", "critical"]
