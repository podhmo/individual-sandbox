import mypy_extensions as mx


class Pair(mx.TypedDict, total=True):
    left: int
    right: int


class Pair2(mx.TypedDict, total=False):
    left: int
    right: int


class Pair3(mx.TypedDict):
    left: int
    right: int


d0: Pair = {"left": 0, "right": 0}
d1: Pair2 = {"left": 0, "right": 0}
d2: Pair = {"left": 0}  # error
d3: Pair2 = {"left": 0}
d4: Pair = {"left": 0}  # error
d5: Pair2 = {"left": 0}
