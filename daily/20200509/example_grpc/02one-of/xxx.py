import typing as t

uint64 = t.NewType("uint64", int)


class X:
    name: str


class Y:
    id: uint64
    name: str


class Node:
    node: t.Union[X, Y]
