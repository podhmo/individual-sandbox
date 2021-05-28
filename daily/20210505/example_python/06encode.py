import enum
import dataclasses
from functools import partial
import jsonpickle


class Color(enum.Enum):
    Red = 1
    Green = 2
    Blue = 3
    Yellow = 4


@dataclasses.dataclass
class Person:
    name: str
    initial: str = dataclasses.field(init=False)
    color: Color

    def __post_init__(self):
        self.initial = self.name.upper()[0]


loads = jsonpickle.decode
dumps = partial(jsonpickle.encode, indent=2)


if __name__ == "__main__":
    p = Person(name="foo", color=Color.Green)
    print(p == loads(dumps(p)))
    print(dumps(p))
    s = """
{
  "py/object": "__main__.Person",
  "name": "foo",
  "color": {
    "py/reduce": [
      {
        "py/type": "__main__.Color"
      },
      {
        "py/tuple": [
          2
        ]
      }
    ]
  },
  "initial": "F"
}
"""
    print(loads(s))
    print(p == loads(dumps(p)))
