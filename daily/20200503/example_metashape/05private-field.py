from emit import emit
from prestring.go.gofmt import gofmt


class Person:
    name: str
    age: int
    _private_field: str  # bytes


print(gofmt(emit([Person]), always=False))
