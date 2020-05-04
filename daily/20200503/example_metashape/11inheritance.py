from emit import emit
from prestring.go.gofmt import gofmt


class A:
    x: int
    y: int


class B(A):
    y: str
    z: str


print(gofmt(emit([B]), always=False))
