from __future__ import annotations
import dataclasses


@dataclasses.dataclass
class Config:
    app: AppConfig
    thirdparty: ThirdpartyConfig


@dataclasses.dataclass
class AppConfig:
    db: str


@dataclasses.dataclass
class ThirdpartyConfig:
    foo_token: str
    bar_token: str
