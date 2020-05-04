from __future__ import annotations
import typing as t
from prestring.go.gofmt import gofmt
from emit import emit


class Person:
    name: str
    memo: Memo


class Memo:
    name: str


print(gofmt(emit([Person]), always=False))
