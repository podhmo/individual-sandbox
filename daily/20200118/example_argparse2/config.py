import typing as t
from pydantic.main import BaseModel


class XXXConfig(BaseModel):
    token: str


class ZZZConfig(BaseModel):
    clientId: str
    clientSecret: str


class ThirdpartyConfig(BaseModel):
    xxx: XXXConfig
    zzz: ZZZConfig


class MainConfig(BaseModel):
    db: str


class Config(BaseModel):
    main: MainConfig
    thirdparty: t.Optional[ThirdpartyConfig] = None
