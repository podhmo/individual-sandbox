import contextlib
from functools import partial


class Fail(Exception):
    pass


block = partial(contextlib.suppress, Fail)


@contextlib.contextmanager
def _if_(cond):
    if not cond:
        raise Fail(cond)
    yield cond


@contextlib.contextmanager
def if_(cond):
    with contextlib.ExitStack() as s:
        s.enter_context(contextlib.suppress(Fail))
        s.enter_context(_if_(cond))
        yield cond  # didn't yield


with if_(True):
    print("ok")

print("----------------------------------------")

with if_(False):
    print("ng")

print("----------------------------------------")
