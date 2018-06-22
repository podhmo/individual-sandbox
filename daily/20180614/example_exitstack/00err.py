from contextlib import ExitStack


def err():
    with ExitStack() as s:
        s.callback(lambda: print("hai"))
        print("before")
        return 1 / 0
        print("after")


err()
