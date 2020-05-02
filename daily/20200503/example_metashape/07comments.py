from emit import emit, field, metadata
from prestring.go.gofmt import gofmt


class Person:
    """
    this is person object

    long description here.
    """

    name: str = field(metadata=metadata(comment="name of person"))
    age: int = field(metadata=metadata(comment="age of person"))


print(gofmt(emit([Person]), always=False))
