from __future__ import annotations
import typing as t
from prestring.go.gofmt import gofmt
from emit import emit


class Person:
    name: str
    skils: t.Dict[str, str]
    children: t.Dict[str, Person]
    children2: t.Dict[str, t.Optional[Person]]
    children3: t.Optional[t.Dict[str, Person]]


print(gofmt(emit([Person]), always=False))
