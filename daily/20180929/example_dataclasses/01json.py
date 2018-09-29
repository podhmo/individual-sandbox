from dataclasses import dataclass, asdict, is_dataclass


@dataclass
class Point:
    x: int
    y: int


import json


def custom_default(o):
    if is_dataclass(o):
        return asdict(o)
    raise TypeError(f"{o!r} is not JSON serializable")


p = Point(x=1, y=2)
print(json.dumps(p, default=custom_default))
# {"x": 1, "y": 2}
print(json.dumps([p, p, p], default=custom_default))
# [{"x": 1, "y": 2}, {"x": 1, "y": 2}, {"x": 1, "y": 2}]
