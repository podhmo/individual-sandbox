from contextlib import ExitStack


class F:
    def __init__(self, name):
        self.name = name
        print("open", self.name)

    def close(self):
        print("close", self.name)


def f():
    with ExitStack() as s:
        f = F("A.txt")
        s.callback(f.close)

        f = F("B.txt")
        s.callback(f.close)


def g():
    with ExitStack() as s:
        f = F("A.txt")
        s.callback(lambda: f.close())

        f = F("B.txt")
        s.callback(lambda: f.close())


f()
print("g")
g()
