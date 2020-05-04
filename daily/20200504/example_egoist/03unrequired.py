from prestring.go.gofmt import gofmt
from emit2 import emit
from emit2 import metadata, field


class Person:
    name: str
    nickname: str = field(metadata=metadata(required=False))


print(gofmt(emit([Person]), always=False))
