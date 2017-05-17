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


class C(A, B):
    def m(self):
        return (self.f(), self.i())
