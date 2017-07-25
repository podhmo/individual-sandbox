from functools import wraps
from contextlib import _GeneratorContextManager


class MyContextManager(_GeneratorContextManager):
    def __exit__(self, type, value, traceback):
        try:
            next(self.gen)
        except StopIteration:
            if type is None:
                return
        else:
            if type is None:
                raise RuntimeError("generator didn't stop")
        return super().__exit__(type, value, traceback)


def contextmanager(func):
    @wraps(func)
    def helper(*args, **kwds):
        return MyContextManager(func, args, kwds)

    return helper


@contextmanager
def ob():
    ob = object()
    print("")
    print("<<< before", id(ob))
    yield ob
    print(">>> after", id(ob))


# with error
def test_it(ob):
    print("**test", id(ob), "**")
    1 / 0


try:
    with ob() as x:
        test_it(x)
except Exception as e:
    print(e)


# normal
def test_it2(ob):
    print("**test", id(ob), "**")


try:
    with ob() as x:
        test_it2(x)
except Exception as e:
    print(e)
