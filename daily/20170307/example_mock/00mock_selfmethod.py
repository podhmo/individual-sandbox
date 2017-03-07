from functools import partial
import unittest.mock as mock


class A(object):
    def f(self):
        print("f", self)
        return self.g()

    def g(self):
        print("g", self)
        return ["g"]


def create_mock():
    m = mock.Mock(spec_set=A)
    m.f = partial(A.f, m)
    return m


def handmade_inheritance_mock(cls, attributes):
    m = mock.Mock(spec_set=cls)
    m.configure_mock(**{k: partial(getattr(cls, k), m) for k in attributes})
    return m


print("A----------------------------------------")
a = A()
print("return", a.f())

print("mock A----------------------------------------")
ma = create_mock()
ma.g.return_value = ["*mock*"]
print("return", ma.f())

print("mock A2----------------------------------------")
ma2 = handmade_inheritance_mock(A, ["f"])
ma2.g.return_value = ["*mock2*"]
print("return", ma2.f())

print("mock A3----------------------------------------")
with mock.patch.object(A, "g", spec_set=A) as mg:
    mg.return_value = ["*mock3*"]
    a = A()
    print("return", a.f())

# A----------------------------------------
# f <__main__.A object at 0x10c634a58>
# g <__main__.A object at 0x10c634a58>
# return ['g']
# mock A----------------------------------------
# f <Mock spec_set='A' id='4504612480'>
# return ['*mock*']
# mock A2----------------------------------------
# f <Mock spec_set='A' id='4506004168'>
# return ['*mock2*']
