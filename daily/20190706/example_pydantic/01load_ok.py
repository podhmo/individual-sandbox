from pydantic import BaseModel


class Person(BaseModel):
    name: str
    age: int


person = Person(**{"name": "foo", "age": 20})
print(person)
