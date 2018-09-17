class A:
    def f(self):
        print("a")
        self.g()

    def g(self):
        raise NotImplementedError("hmm")


class B(A):
    def g(self):
        print("b")


class C(A):
    def g(self):
        print("c")


C().f()
