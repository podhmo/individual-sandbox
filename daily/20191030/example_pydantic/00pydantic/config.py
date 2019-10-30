from __future__ import annotations
import typing as t
import typing_extensions as tx
import pydantic

"""
    port:      uint16
    logLevel:  "debug" | *"info" | "warn" | "error" | "critical"
    secondary: <self>
    xxxAPI:
      token: string
"""


class Config(pydantic.BaseModel):
    port: int
    logLevel: tx.Literal["debug", "info", "warn", "error", "critical"]

    secondary: t.Optional[Config]
    xxxAPI: t.Optional[XXXAPI]


class XXXAPI(pydantic.BaseModel):
    token: str


Config.update_forward_refs()
