from dataclasses import dataclass
import json


@dataclass(frozen=True)
class Person:
    name: str
    age: int
    nickname: str = ""


def make_person(pairs):
    return Person(**pairs)


s = """{"name": "foo", "age": 20}"""
p = json.loads(s, object_hook=make_person)
print(p)
