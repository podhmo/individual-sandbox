class A:
    def f(self):
        return "f"

    def g(self):
        return "g"


class B:
    def h(self):
        return "h"

    def i(self):
        return "i"


class C:
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def m(self):
        return (self.a.f(), self.b.i())
