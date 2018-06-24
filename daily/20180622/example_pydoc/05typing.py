"""
print sinagure with typing
"""
import pydoc
import sys


def f(x: int, y: int) -> int:
    return x + y


print(pydoc.plaintext.document(sys.modules[__name__]))
