import dataclasses
import enum
import jsonpickle


class Status(enum.Enum):
    x = enum.auto()
    y = enum.auto()
    z = enum.auto()


@dataclasses.dataclass
class A:
    name: str
    age: int
    status: Status


a = A(name="foo", age=0, status=Status.y)
print(jsonpickle.encode(a))
print(jsonpickle.decode(jsonpickle.encode(a)))
print(jsonpickle.decode(jsonpickle.encode(a)) == a)
