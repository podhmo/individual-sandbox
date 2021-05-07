import enum
import json
import dataclasses


class Color(enum.Enum):
    Red = 1
    Green = 2
    Blue = 3
    Yellow = 4


@dataclasses.dataclass
class Person:
    name: str
    color: Color


def dump(ob):
    def _default(ob):
        if dataclasses.is_dataclass(ob):
            return {
                "__dataclass__": {
                    "type": ob.__class__.__name__,
                    "value": dataclasses.asdict(ob),
                }
            }
        elif isinstance(ob, enum.Enum):
            return {"__enum__": str(ob)}
        raise TypeError(f"unexpected type {ob!r}")

    return json.dumps(ob, indent=2, default=_default)


def load(s):
    def _on_pair(itr):
        d = {k: v for k, v in itr}
        if "__enum__" in d:
            clsname, attr = d["__enum__"].split(".", 2)
            return getattr(globals()[clsname], attr)
        elif "__dataclass__" in d:
            v = d["__dataclass__"]
            return globals()[v["type"]](**v["value"])
        else:
            return d

    return json.loads(s, object_pairs_hook=_on_pair)


print(dump(Person(name="foo", color=Color.Green)))

s = """
{
  "__dataclass__": {
    "type": "Person",
    "value": {
      "name": "foo",
      "color": {
        "__enum__": "Color.Green"
      }
    }
  }
}
"""
print(load(s))
