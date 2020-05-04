from __future__ import annotations
import typing as t
from prestring.go.gofmt import gofmt
from emit import emit


class Person:
    name: str
    father: t.Optional[Person]
    mother: t.Optional[Person]


print(gofmt(emit([Person]), always=False))
