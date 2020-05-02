from __future__ import annotations
from emit import emit
from prestring.go.gofmt import gofmt
from egoist.go.types import gopackage


class Person:
    name: str
    age: int
    memo: Memo


@gopackage("m/memo")
class Memo:
    memo: str


print(gofmt(emit([Person]), always=False))
