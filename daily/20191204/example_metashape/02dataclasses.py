import dataclasses
from metashape.runtime import get_walker


@dataclasses.dataclass
class Person:
    name: str


w = get_walker(Person)
for cls in w.walk():
    print(cls)

w = get_walker(Person(name="foo"))
for cls in w.walk():
    print(cls)
