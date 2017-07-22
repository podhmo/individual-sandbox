import contextlib


@contextlib.contextmanager
def f():
    # setup
    print("setup f")
    yield
    # teardown
    print("teardown f")


@contextlib.contextmanager
def g():
    # setup
    print("setup g")
    yield
    # teardown
    print("teardown g")


@contextlib.contextmanager
def h():
    # setup
    print("setup h")
    yield
    # teardown
    print("teardown h")


@contextlib.contextmanager
def nested(cms):
    with contextlib.ExitStack() as s:
        for cm in cms:
            s.enter_context(cm())
        yield


with nested([f, g, h]):
    print("** do something**")
