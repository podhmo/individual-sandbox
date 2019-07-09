import typing as t
from pydantic import BaseModel
from fastapi.encoders import jsonable_encoder
from faker import Faker

fake = Faker()


class X(BaseModel):
    x: t.Any


obj = X(x=fake.name)
print(jsonable_encoder(obj))
print(obj.dict())
