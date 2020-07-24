import typing_extensions as tx
from named import named


YYY = tx.Literal["z", "y"]
XXX = named("XXX", tx.Literal["x", "y"])


def foo(xxx: XXX) -> None:
    reveal_type(xxx)
    print(xxx)


foo("x")  # ok
foo("z")  # ng
