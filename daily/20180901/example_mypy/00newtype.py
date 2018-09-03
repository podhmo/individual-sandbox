import typing as t

XYZ = t.NewType("XYZ", str)


def f(v: XYZ) -> None:
    print(v)


f(XYZ("foo"))
