import contextlib


@contextlib.contextmanager
def noisy():
    print("start")
    yield
    print("end")


with contextlib.suppress(ZeroDivisionError):
    with noisy():
        1 / 0
