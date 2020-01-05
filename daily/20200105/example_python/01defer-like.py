import contextlib


@contextlib.contextmanager
def noisy():
    print("start")
    try:
        yield
    finally:
        print("end")


with contextlib.suppress(ZeroDivisionError):
    with noisy():
        1 / 0
