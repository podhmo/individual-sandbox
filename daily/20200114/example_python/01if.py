import contextlib
from functools import wraps


class Fail(Exception):
    pass


class _GeneratorContextManager(contextlib._GeneratorContextManager):
    def __exit__(self, type, value, traceback):
        try:
            super().__exit__(type, value, traceback)
        except RuntimeError as e:
            if e.args[0] != "generator didn't yield":
                raise


def contextmanager(func):
    @wraps(func)
    def helper(*args, **kwds):
        return _GeneratorContextManager(func, args, kwds)

    return helper


@contextmanager
def if_(cond):
    if cond:
        yield cond


with if_(False):
    print("ng", cond)
