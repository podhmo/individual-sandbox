import pydantic


class Config(pydantic.BaseModel):
    name: str
