from collections import namedtuple


A = namedtuple("A", "x y")
B = namedtuple("B", "i j")


print(A(x=10, y=20) == B(i=10, j=20))
