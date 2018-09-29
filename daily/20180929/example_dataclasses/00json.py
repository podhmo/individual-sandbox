from dataclasses import dataclass, asdict


@dataclass
class Point:
    x: int
    y: int


import json
p = Point(x=1, y=2)
print(json.dumps(p))
