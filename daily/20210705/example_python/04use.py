from dataclasses import dataclass
import configlang

@dataclass
class Point:
    x: int
    y: int


@dataclass
class Pair:
    left: Point
    right: Point


def run(y: int) -> dict:
    p0 = Point(x=10, y=y)
    pair = f(y)
    p1 = Point(x=20, y=y)


def f(y):
    p0 = Point(x=10, y=y)
    p1 = Point(x=20, y=y)
    return Pair(p0, p1)

print(configlang.run_dsl(run))
# {'y': 20, 'p0': Point(x=10, y=20), 'pair': Pair(left=Point(x=10, y=20), right=Point(x=20, y=20)), 'p1': Point(x=20, y=20)}