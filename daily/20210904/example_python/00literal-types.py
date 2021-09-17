from typing import Iterable, Tuple, TypeVar

K = TypeVar("K")
V = TypeVar("V")


def get_first(source: Iterable[Tuple[K, V]]) -> Tuple[K, V]:
    ...


def main() -> None:
    a = get_first([(1, 2)])
    b: Tuple[int, int] = get_first([(1, 10)])
    a == b

    reveal_type(a)
    reveal_type(b)
    reveal_locals()
