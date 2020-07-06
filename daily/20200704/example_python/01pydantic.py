import typing as t
import pydantic


class PersonConfig(pydantic.BaseModel):
    name: str


class Config:
    xxx: PersonConfig
    yyy: PersonConfig


print(t.get_type_hints(Config))
