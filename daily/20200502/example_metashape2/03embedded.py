from __future__ import annotations
import typing as t
from emit import emit, metadata, field
from prestring.go.gofmt import gofmt


class Person:
    name: str
    age: int
    memo: Memo = field(metadata=metadata(inline=True))


class Memo:
    data: t.Dict[str, t.Any]


print(gofmt(emit([Person]), always=False))
