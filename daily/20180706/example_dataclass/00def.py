from dataclasses import dataclass


@dataclass(frozen=True)
class Person:
    name: str
    age: int
    nickname: str = ""


p = Person(name="foo", age=20)
p2 = Person(name="foo", age=20)
print(p == p2)
