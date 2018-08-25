import typing_extensions as tx


class F(tx.Protocol):
    def __call__(self, x: int, y: int, *, verbose: bool = False) -> int:
        ...


def add(x: int, y: int, *, verbose: bool = False) -> int:
    return x + y


def use(f: F, x: int, y: int) -> int:
    return f(x, y)


def main() -> None:
    print(use(add, 10, 20))
