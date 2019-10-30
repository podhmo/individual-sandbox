from __future__ import annotations
import typing as t
import typing_extensions as tx
from pydantic import dataclasses

"""
    port:      uint16
    log_level:  "debug" | *"info" | "warn" | "error" | "critical"
    secondary: <self>
    xxxAPI:
      token: string
"""


@dataclasses.dataclass
class Config:
    port: int
    log_level: tx.Literal["debug", "info", "warn", "error", "critical"] = "info"
    # secondary: t.Optional[Config] = None
    # xxxAPI: t.Optional[XXXAPI] = None


# XXX: not support Config.update_forward_refs() ?


@dataclasses.dataclass
class XXXAPI:
    token: str
