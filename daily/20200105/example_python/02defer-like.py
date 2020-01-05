import contextlib

with contextlib.suppress(ZeroDivisionError):
    with contextlib.ExitStack() as s:
        print("start")
        s.callback(lambda: print("end"))
        1 / 0
