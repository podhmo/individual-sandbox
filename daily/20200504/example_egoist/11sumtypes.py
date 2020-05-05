# https://eli.thegreenplace.net/2018/go-and-algebraic-data-types/
from __future__ import annotations
import typing as t
from prestring.go.gofmt import gofmt
from emit import emit, field, metadata


class Empty:
    pass


class Leaf:
    value: int


class Node:
    left: Tree
    right: Tree


Tree = t.Union[Empty, Leaf, Node]
Tree.__name__ = "Tree"


print(gofmt(emit([Tree]), always=False))
