class A:
    def f(self):
        print("@@", self)
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
# @@ <__main__.C object at 0x7f3519c80630>
# a
# c

