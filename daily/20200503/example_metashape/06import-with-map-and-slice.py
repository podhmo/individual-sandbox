from __future__ import annotations
import typing as t
from emit import emit
from prestring.go.gofmt import gofmt
from egoist.go.types import gopackage


class Person:
    name: str
    age: int
    parents: t.List[Person]
    memo: t.Dict[str, Memo]


@gopackage("m/memo")
class Memo:
    pass


print(gofmt(emit([Person]), always=False))
