from prestring.go.gofmt import gofmt
from emit2 import emit


class Person:
    name: str


print(gofmt(emit([Person]), always=False))
