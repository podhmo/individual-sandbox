import typing as t


class X:
    name: str
    value: str


class Y:
    value: int


XY = t.Union[X, Y]

