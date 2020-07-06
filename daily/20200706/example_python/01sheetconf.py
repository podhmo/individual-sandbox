import typing_extensions as tx
import sheetconf
from pydantic import BaseModel, Field


class XXXConfig(BaseModel):
    name: str
    token: str


class YYYConfig(BaseModel):
    name: str = Field(default="yyy", description="name of yyy")
    token: str = Field(description="token of yyy")


class LoggerConfig(BaseModel):
    level: tx.Literal["DEBUG", "INFO", "WARNING", "ERROR"]


class Config(BaseModel):
    xxx: XXXConfig
    yyy: YYYConfig
    logger: LoggerConfig


if __name__ == "__main__":
    url = "config-csv"
    config = sheetconf.loadfile(url, config=Config, format="csv", adjust=True)

    print(config)
