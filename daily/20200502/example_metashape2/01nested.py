from __future__ import annotations
import typing as t
from emit import emit
from prestring.go.gofmt import gofmt


class Person:
    name: str
    age: int
    father: t.Optional[Person]
    mother: t.Optional[Person]


print(gofmt(emit([Person]), always=False))
