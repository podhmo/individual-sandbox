import contextlib
from functools import partial


class Fail(Exception):
    pass


block = partial(contextlib.suppress, Fail)


@contextlib.contextmanager
def if_(cond):
    if not cond:
        raise Fail(cond)
    yield cond


with block():
    with if_(True):
        print("ok")
    print("reached")


with block():
    with if_(False):
        print("ng")
    print("never")

print("----------------------------------------")
with contextlib.ExitStack() as s:
    s.enter_context(block())
    s.enter_context(if_(True))
    print("ok")

with contextlib.ExitStack() as s:
    s.enter_context(block())
    s.enter_context(if_(False))
    print("ng")
