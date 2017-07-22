import contextlib


@contextlib.contextmanager
def f():
    # setup
    print("setup f")
    yield
    # teardown
    print("setup f")


@contextlib.contextmanager
def g():
    # setup
    print("setup g")
    yield
    # teardown
    print("setup g")


@contextlib.contextmanager
def h():
    # setup
    print("setup h")
    yield
    # teardown
    print("setup h")


@contextlib.contextmanager
def nested(cms):
    if not cms:
        yield
    else:
        with cms[0]():
            with nested(cms[1:]):
                yield


with nested([f, g, h]):
    print("** do something**")
