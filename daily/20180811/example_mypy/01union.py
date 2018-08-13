import typing as t

X = t.Callable[[int], None]
Y = t.Callable[[str], None]
G = t.Union[X, Y]


def f0(g: G) -> None:
    # hmm:
    if isinstance(g, t.Callable[[int], None]):
        g(0)
