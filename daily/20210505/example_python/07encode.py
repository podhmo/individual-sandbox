import enum
import dataclasses
from functools import partial
import jsonpickle


loads = jsonpickle.decode
dumps = partial(jsonpickle.encode, indent=2)

if __name__ == "__main__":

    class Color(enum.Enum):
        Red = 1
        Green = 2
        Blue = 3
        Yellow = 4

    @dataclasses.dataclass
    class W:
        name: str

    @dataclasses.dataclass
    class Person:
        name: str
        initial: str = dataclasses.field(init=False)
        color: Color
        w: W = dataclasses.field(init=False)
        w0: W

        def __post_init__(self):
            self.initial = self.name.upper()[0]
            self.w = W(name=self.name.upper())

    p = Person(name="foo", color=Color.Green, w0=W(name="xxx"))
    p.initial = "G"
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
  "w0": {
    "py/object": "__main__.W",
    "name": "xxx"
  },
  "initial": "G",
  "w": {
    "py/object": "__main__.W",
    "name": "FOO"
  }
}
"""
    print(loads(s))
    print(p == loads(dumps(p)))
