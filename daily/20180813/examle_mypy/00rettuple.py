import typing as t


def f() -> t.Tuple[int, ...]:
    return 1, 2, 3


def g() -> t.Tuple[str, int, ...]:
    return "foo", 1, 2, 3


# hmm t.Tuple[str, int, t.Any] ...
