import warnings
import sys
import functools
import contextlib

_debug = True


def setdebug(value):
    global _debug
    _debug = value


def getdebug():
    global _debug
    return _debug


def shouldenter(fn, level=2):
    @functools.wraps(fn)
    def _shouldenter(*args, **kwargs):
        return ShouldEnter(fn(*args, **kwargs), level=level)

    return _shouldenter


class ShouldEnter:
    def __init__(self, internal=None, level=1, debug=False, message_class=UserWarning):
        self.internal = internal
        self.message_class = message_class
        self.used = False

        self.lineno = 0
        self.filename = None

        if debug or getdebug():
            # get context information for warning message
            f = sys._getframe(level)
            self.lineno = f.f_lineno
            self.filename = f.f_code.co_filename

    def __enter__(self):
        self.used = True
        if hasattr(self.internal, "__enter__"):
            return self.internal.__enter__()
        else:
            return self.internal

    def __exit__(self, exc_type, exc_value, traceback):
        if hasattr(self.internal, "__exit__"):
            return self.internal.__exit__(exc_type, exc_value, traceback)
        else:
            return None

    def __del__(self):
        if not self.used:
            self.warn()

    def warn(self):
        msg = "should use as context manager, (__enter__() is not called)"
        if self.filename is None:
            warnings.warn(msg, self.message_class)
        else:
            warnings.warn_explicit(msg, self.message_class, self.filename, self.lineno)


def f():
    g(10)


def g(x):
    with h() as message:
        print(message)
    h()


@shouldenter
@contextlib.contextmanager
def h():
    print("before")
    yield "hai"
    print("after")


f()
