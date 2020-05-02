from emit import emit
from prestring.go.gofmt import gofmt


class Person:
    name: str
    age: int

    class X:
        name: str

        class Y:
            name: str

        y: Y

    x: X


print(gofmt(emit([Person]), always=False))
