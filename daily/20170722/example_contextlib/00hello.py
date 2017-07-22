import contextlib


@contextlib.contextmanager
def f():
    # setup
    print("do something for setup")
    yield
    # teardown
    print("do something for teardown")


with f():
    print("hello")
