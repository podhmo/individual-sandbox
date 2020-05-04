from __future__ import annotations
import typing as t
from prestring.go.gofmt import gofmt
from emit import emit, field, metadata


class Person:
    name: str
    memo: Memo


class X:
    name: str
    xxx: str


class Y:
    name: str
    yyy: str


Memo = t.Union[X, Y]
Memo.__name__ = "Memo"


print(gofmt(emit([Person]), always=False))
