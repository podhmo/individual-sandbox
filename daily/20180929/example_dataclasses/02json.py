from dataclasses import dataclass, asdict, is_dataclass


@dataclass
class Point:
    x: int
    y: int


import extjson as json

p = Point(x=1, y=2)
print(json.dumps(p))
# {"x": 1, "y": 2}
print(json.dumps([p, p, p]))
# [{"x": 1, "y": 2}, {"x": 1, "y": 2}, {"x": 1, "y": 2}]
