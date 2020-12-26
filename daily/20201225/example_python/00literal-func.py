from __future__ import annotations
import typing_extensions as tx


def foo() -> None:
    pass


def bar() -> None:
    pass


def use(fn: tx.Literal[foo, bar]) -> None:
    pass

# 00literal-func.py:13: error: Parameter 1 of Literal[...] is invalid
# 00literal-func.py:13: error: Function "00literal-func.foo" is not valid as a type
# 00literal-func.py:13: note: Perhaps you need "Callable[...]" or a callback protocol?
