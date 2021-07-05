from dataclasses import dataclass


@dataclass
class Point:
    x: int
    y: int


@dataclass
class Pair:
    left: Point
    right: Point


def run(y:int) -> dict:
    _exception = None
    try:
        p0 = Point(x=10, y=y)
        p1 = Point(x=20, y=y)
    except Exception as exc:
        _exception = exc
    finally:
        return locals()

print(run(20))