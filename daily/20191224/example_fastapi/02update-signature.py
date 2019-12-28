import typing as t
from functools import wraps
from fastapi.dependencies.utils import get_typed_signature


def hello() -> t.Dict[str, str]:
    return {"hello": "world"}


def foo(fn):
    def _foo(*args, **kwargs):
        return fn(*args, **kwargs)

    return _foo


def bar(fn):
    @wraps(fn)
    def _bar(*args, **kwargs):
        return fn(*args, **kwargs)

    return _bar


def boo(fn):
    def _boo(*args, **kwargs):
        return fn(*args, **kwargs)

    _boo.__wrapped__ = fn
    return _boo


def yoo(fn):
    def _yoo(*args, **kwargs):
        return fn(*args, **kwargs)

    def heke(x: int, y: int) -> None:
        pass

    _yoo.__wrapped__ = heke
    return _yoo


print("normal", get_typed_signature(hello))
print("decorated", get_typed_signature(foo(hello)))
print("decorated with wraps", get_typed_signature(bar(hello)))
print("decorated with assign", get_typed_signature(boo(hello)))
print("decorated with fake", get_typed_signature(yoo(hello)), yoo(hello)())
