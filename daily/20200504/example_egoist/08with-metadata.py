from __future__ import annotations
import typing as t
from prestring.go.gofmt import gofmt
from emit import emit, field, metadata


class Person:
    name: str
    children: t.List[Person] = field(metadata=metadata(required=False))
    children2: t.List[t.Optional[Person]] = field(metadata=metadata(required=False))


print(gofmt(emit([Person]), always=False))
