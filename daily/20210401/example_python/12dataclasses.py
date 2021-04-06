import sys
import enum
import json
import dataclasses
from importlib import import_module


def dumps(ob):
    return json.dumps(ob, indent=2, default=_default)


def loads(s):
    return json.loads(s, object_pairs_hook=_on_pairs)


def _default(ob):
    if dataclasses.is_dataclass(ob):
        d = {}
        additionals = {}
        for f in dataclasses.fields(ob):
            if not f.init:
                additionals[f.name] = getattr(ob, f.name)
            else:
                d[f.name] = getattr(ob, f.name)

        r = {
            "type": f"{ob.__class__.__module__}.{ob.__class__.__name__}",
            "value": d,
        }
        if additionals:
            r["additionals"] = additionals
        return {"__dataclass__": r}
    elif isinstance(ob, enum.Enum):
        return {
            "__enum__": f"{ob.__class__.__module__}.{ob.__class__.__name__}.{ob.name}",
        }
    raise TypeError(f"unexpected type {ob!r}")


def _on_pairs(itr):
    d = {k: v for k, v in itr}
    if "__enum__" in d:
        module, clsname, attr = d["__enum__"].rsplit(".", 3)
        m = sys.modules.get(module)
        if m is None:
            m = import_module(module)
        cls = getattr(m, clsname)
        return getattr(cls, attr)
    elif "__dataclass__" in d:
        v = d["__dataclass__"]
        module, clsname = v["type"].rsplit(".", 2)
        m = sys.modules.get(module)
        if m is None:
            m = import_module(module)
        cls = getattr(m, clsname)
        ob = cls(**v["value"])
        if "additionals" in v:
            for k, v in v["additionals"].items():
                setattr(ob, k, v)
        return ob
    else:
        return d


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
  "__dataclass__": {
    "type": "__main__.Person",
    "value": {
      "name": "foo",
      "color": {
        "__enum__": "__main__.Color.Green"
      },
      "w0": {
        "__dataclass__": {
          "type": "__main__.W",
          "value": {
            "name": "xxx"
          }
        }
      }
    },
    "additionals": {
      "initial": "G",
      "w": {
        "__dataclass__": {
          "type": "__main__.W",
          "value": {
            "name": "FOO"
          }
        }
      }
    }
  }
}
"""
    print(loads(s))
    print(p == loads(dumps(p)))
