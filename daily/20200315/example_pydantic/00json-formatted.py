from pydantic import BaseModel


class Person(BaseModel):
    name: str
    age: int


p = Person(name="foo", age=20)
print(p.json(indent=2))
