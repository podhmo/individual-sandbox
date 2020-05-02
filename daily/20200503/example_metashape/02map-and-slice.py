from __future__ import annotations
import typing as t
from emit import emit
from prestring.go.gofmt import gofmt


class Person:
    name: str
    age: int
    parents: t.List[Person]
    memo: t.Dict[str, t.Any]


print(gofmt(emit([Person]), always=False))
