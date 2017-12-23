import warnings
import contextlib


class ShouldEnter(contextlib.AbstractContextManager):
    def __init__(self):
        self.used = False

    def __enter__(self):
        self.used = True

    def __exit__(self, exc_type, exc_value, traceback):
        return None

    def __del__(self):
        if not self.used:
            warnings.warn(
                "should use as context manager, (__enter__() is not called)", stacklevel=0
            )
            warnings.warn_explicit("hai", UserWarning, "00shouldenter.py", 16)


def g(x):
    with ShouldEnter():
        pass
    ShouldEnter()


def f():
    g(10)


f()
