from collections import namedtuple


A = namedtuple("A", "x y")
B = namedtuple("B", "i j")


class BPlus(B):
    def hai(self):
        return "hai"


def equal(a, b):
    is_same_class = issubclass(a.__class__, b.__class__) or issubclass(b.__class__, a.__class__)
    return is_same_class and a == b


print(equal(A(10, 20), A(10, 20)))
print(equal(A(10, 20), B(10, 20)))
print(equal(A(10, 20), BPlus(10, 20)))
print(equal(B(10, 20), BPlus(10, 20)))
print(equal(BPlus(10, 20), B(10, 20)))
