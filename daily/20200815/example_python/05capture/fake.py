from capture import Capture
from hello import hello
from functools import wraps

hello = Capture(hello)  # type:ignore
__all__ = ["hello"]
