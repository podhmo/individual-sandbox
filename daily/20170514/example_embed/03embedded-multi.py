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


missing = object()


class C:
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def m(self):
        return (self.a.f(), self.b.i())

    def __getattr__(self, name):
        v = getattr(self.a, name, missing)
        if v is not missing:
            return v
        return getattr(self.b, name)

print(C(A(), B()).i())
