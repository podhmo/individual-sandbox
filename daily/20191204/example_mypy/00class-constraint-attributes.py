class A:
    x: int
    y: int
    __slots__ = ("x", "y")


class B(A):
    z: int
