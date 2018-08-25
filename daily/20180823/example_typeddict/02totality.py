import mypy_extensions as mx


class Pair(mx.TypedDict, total=False):
    left: int
    right: int


d0: Pair = {"left": 0, "right": 0}
d1: Pair = {"left": 0}
d2: Pair = {}
