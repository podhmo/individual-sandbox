def f(x: int) -> int:
    return g(x)  # error


def g(x: int, y: int) -> int:
    return g + y
