from prestring.go.gofmt import gofmt
from emit import emit


class Person:
    name: str


print(gofmt(emit([Person]), always=False))
