from __future__ import annotations
import typing as t
from prestring.go.gofmt import gofmt
from emit import emit, field, metadata


class Person:
    name: str
    skils: t.List[str]
    children: t.List[Person]
    children2: t.List[t.Optional[Person]]
    children3: t.Optional[t.List[Person]]


print(gofmt(emit([Person]), always=False))
