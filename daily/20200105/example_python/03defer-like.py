import contextlib


@contextlib.contextmanager
def noisy():
    print("start")
    yield
    print("end")


with contextlib.suppress(ZeroDivisionError):
    with contextlib.ExitStack() as s:
        s.enter_context(noisy())
        1 / 0
