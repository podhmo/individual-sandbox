import logging
from strangeeval import evaluate, DEBUG


class Point:
    x: int
    y: int


def use(*, x: int) -> None:
    lpt = make_point(x)
    print("lpt == ", lpt)

    rpt = make_point(x + 10)
    pair = (lpt, rpt)
    return pair


def make_point(x: int, *, y: int = 20) -> Point:
    return Point(x=x, y=20)


log_level = logging.DEBUG if DEBUG else logging.INFO
logging.basicConfig(level=log_level)
# use(x=10)
print(evaluate(use, x=10))