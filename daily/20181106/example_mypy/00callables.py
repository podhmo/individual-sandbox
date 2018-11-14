import typing as t


def f(xs: t.Tuple[int, ...]) -> None:
    pass


def g(xs: t.Tuple[int, str, ...]) -> None:
    pass


if t.TYPE_CHECKING:
    print(reveal_type(f))
    print(reveal_type(g))
