import typing as t
import mypy_extensions as mx


def call(f: t.Callable[[mx.VarArg(str)], str], *args: str) -> str:
    return f(*args)


def merge(*path: str) -> str:
    return "/".join(path)


print(call(merge, "a"))
print(call(merge, "a", "b"))
print(call(merge, "a", "b", "c"))
# call(sum, 1, 2, 3)
