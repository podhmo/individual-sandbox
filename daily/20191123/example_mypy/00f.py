import typing as t


def f(x: t.Optional[int] = None) -> None:
    x = x or 10
    print(x * x)
