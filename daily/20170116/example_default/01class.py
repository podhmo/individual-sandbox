from collections import namedtuple
RGB = namedtuple("RGB", "r g b")


class A(object):
    def __init__(self, r=100, g=100, b=100):
        self.rgb = RGB(r=r, g=g, b=b)


class B(object):
    R = 100
    G = 100
    B = 100

    def __init__(self, r=None, g=None, b=None):
        r = r if r is not None else self.__class__.R
        g = g if g is not None else self.__class__.G
        b = b if b is not None else self.__class__.B
        self.rgb = RGB(r=r, g=g, b=b)


class C(B):
    R = 200


class Hmm:
    strict = True

    def __init__(self, strict):
        self.strict = strict or self.__class__.strict

print(Hmm(strict=False).strict)  # True
print(Hmm(strict=True).strict)  # True


class Hmm2:
    def __init__(self, strict=True):
        self.strict = strict

print(Hmm2(strict=False).strict)  # False
print(Hmm2(strict=True).strict)  # True


from functools import partial
C = partial(A, r=100, g=200, b=0)
print(C(b=1).rgb)  # RGB(r=100, g=200, b=1)
