from contextlib import ExitStack, contextmanager


class Ref:
    def __init__(self, name, *, illed):
        self.name = name
        self.illed = illed
        print("open", self.name)

    def __del__(self):
        if self.illed:
            raise Exception("illed")

    def close(self):
        print("close", self.name)

    def adjust(self):
        print("adjustment for", self.name)
        self.illed = False


def f(name, *, illed):
    with ExitStack() as s:
        ref = Ref(name, illed=illed)
        s.callback(ref.close)
        if ref.illed:
            s.callback(ref.adjust)
        print("use", ref.name)


def g(name, *, illed):
    ref = Ref(name, illed=illed)
    with after(ref, "close"):
        if ref.illed:
            with after(ref, "adjust"):
                print("use", ref.name)
        else:
            print("use", ref.name)


@contextmanager
def after(ob, name):
    try:
        yield ob
    finally:
        getattr(ob, name)()


f("A.txt", illed=False)
print("-")
f("B.txt", illed=True)
print("----------------------------------------")
g("A.txt", illed=False)
print("-")
g("B.txt", illed=True)
