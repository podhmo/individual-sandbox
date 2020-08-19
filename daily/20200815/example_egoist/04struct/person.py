from __future__ import annotations
import typing as t


class Person:
    name: str
    age: int
    father: t.Optional[Person]
    mother: t.Optional[Person]
    children: t.List[Person]
    followings: t.List[t.Optional[Person]]
