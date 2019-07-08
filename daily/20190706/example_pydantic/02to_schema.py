from pydantic import BaseModel


class Person(BaseModel):
    name: str
    age: int


print(Person.schema())
print(Person.schema_json())
