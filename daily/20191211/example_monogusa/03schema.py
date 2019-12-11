from pydantic import BaseModel
from pydantic.fields import ModelField
from pydantic.schema import schema, field_schema


class Message(BaseModel):
    message: str


class Wrap(BaseModel):
    message: Message


print(schema([Message]))
print(field_schema(Message.__fields__["message"], model_name_map={}))

print(schema([Wrap]))
print(Wrap.__fields__)
print(field_schema(Wrap.__fields__["message"], model_name_map={Message: "Message"}))
