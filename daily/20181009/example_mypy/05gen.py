import typing as t
import lib05


def main() -> None:
    reveal_type(lib05.zero(lib05.User))
    reveal_type(lib05.User)
    xs = lib05.zero(lib05.User)
    lib05.use(xs)
