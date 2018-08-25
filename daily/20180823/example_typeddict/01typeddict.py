import mypy_extensions as mx


class Pair(mx.TypedDict):
    left: int
    right: int


d0: Pair = {"left": 0, "right": 0}
