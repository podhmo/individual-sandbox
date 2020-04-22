import contextlib
import sys


class Attr:
    def __init__(self, d):
        self.__dict__.update(d)


def foo(name: str):
    print("foo", name)


def bar(name: str):
    print("bar start", name)
    with context() as c:
        print("bar inner", c.name)
    print("bar end", name)


@contextlib.contextmanager
def context():
    ff = sys._getframe(2)
    yield Attr({name: object() for name in ff.f_code.co_varnames})


foo("xxx")
bar("xxx")
