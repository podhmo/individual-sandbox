from __future__ import annotations
import typing as t
from emit import emit
from prestring.go.gofmt import gofmt


class Atom:
    kind: str


class Composite:
    kind: str
    args: t.List[Node]


Node = t.Union[Atom, Composite]
Node.__name__ = "Node"
print(gofmt(emit([Node]), always=False))
