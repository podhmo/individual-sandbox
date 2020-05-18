import typing as t

T = t.TypeVar("T")


def return_int() -> int:
    return 0


def return_float() -> float:
    return 0.0


def shared(fn: t.Callable[[], T]) -> t.Callable[[], T]:
    from functools import lru_cache

    @lru_cache(1)
    def cached() -> T:
        return fn()

    return cached


reveal_type(return_float)
reveal_type(return_int)
reveal_type(shared(return_float))
reveal_type(shared(return_int))
reveal_type(shared(return_int)())
