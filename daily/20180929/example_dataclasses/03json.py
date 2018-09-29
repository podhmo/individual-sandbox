from dataclasses import dataclass, asdict, is_dataclass


@dataclass
class Point:
    x: int
    y: int


import extjson2 as json

p = Point(x=1, y=2)
print(json.dumps(p))
# {"x": 1, "y": 2}
print(json.dumps([p, p, p]))
print(json.dumps(iter([p, p, p])))
# [{"x": 1, "y": 2}, {"x": 1, "y": 2}, {"x": 1, "y": 2}]
