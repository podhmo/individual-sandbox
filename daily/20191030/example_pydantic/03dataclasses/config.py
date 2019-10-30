import typing_extensions as tx
import pydantic
from pydantic import dataclasses

"""
    port:      uint16
    log_level:  "debug" | *"info" | "warn" | "error" | "critical"
"""


@dataclasses.dataclass
class Config:
    port: int
    log_level: tx.Literal["debug", "info", "warn", "error", "critical"] = "info"
