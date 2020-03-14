import typing as t
from pydantic import BaseModel


class Person(BaseModel):
    name: str
    age: int
    additionals: t.Dict[str, str] = {}


code = """\
{"name": "foo", "age": 20, "nickname": "F"}
"""

print("input:")
print(code)
data = Person.parse_raw(code)

print("got:")
print(data)

print()
print("output")
print(data.json())
