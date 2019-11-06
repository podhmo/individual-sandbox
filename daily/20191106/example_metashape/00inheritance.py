import typing as t
from metashape._access import iterate_props


class Person:
    name: str
    age: int


class ActualPerson(Person):
    age: t.Optional[int]
    nickname: t.Optional[str]


for prop in iterate_props(ActualPerson):
    print(prop)
