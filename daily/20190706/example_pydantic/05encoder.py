from pydantic import BaseModel
from datetime import datetime


class Person(BaseModel):
    name: str
    age: int
    created_at: datetime


person = Person(name="foo", age=20, created_at=datetime(2010, 1, 1))
print(person)
print(person.dict())
