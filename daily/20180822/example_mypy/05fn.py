import typing as t
import mypy_extensions as mx

# (x: int, y:int, verbose: bool=...) -> int みたいな型
F = t.Callable[[int, int, mx.DefaultArg(bool, "verbose")], int]


#  error: Argument 1 to "use" has incompatible type "Callable[[int, int, DefaultNamedArg(bool, 'verbose')], int]"; expected "Callable[[int, int, bool], int]"
def add(x: int, y: int, *, verbose: bool = False) -> int:
    return x + y


def add2(x: int, y: int, verbose: bool = False) -> int:
    return x + y


def use(f: F, x: int, y: int) -> int:
    return f(x, y)


def main() -> None:
    print(use(add, 10, 20))
    print(use(add2, 10, 20))
