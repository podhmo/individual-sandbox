from __future__ import annotations
import typing as t
import typing_extensions as tx


AnyFunction = t.Callable[..., None]


class Depends:
    def __init__(self, *fns: AnyFunction) -> None:
        self.fns = fns


def main(fn: tx.Annotated[AnyFunction, Depends(foo, bar, boo)]) -> None:
    pass


def foo() -> None:
    pass


def bar() -> None:
    pass


# 01metadata-func.py:14:58 undefined name 'boo'
