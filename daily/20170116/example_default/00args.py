from collections import namedtuple
RGB = namedtuple("RGB", "r g b")


def f(r, g=144, b=144):
    return RGB(r=r, g=g, b=b)

print(f(100))  # RGB(r=100, g=144, b=144)


from functools import partial


g = partial(f, r=100, g=100, b=100)
h = partial(g, b=200)

print(g())  # RGB(r=100, g=100, b=100)
print(g(b=255))  # RGB(r=100, g=100, b=255)
print(h(r=0))  #  RGB(r=0, g=100, b=200)
