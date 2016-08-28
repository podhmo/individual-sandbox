from collections import namedtuple

A = namedtuple("A", "x y")
B = namedtuple("B", "x y")
C = namedtuple("C", "i j")

print(A(x=10, y=20) == A(x=10, y=20))
print(id(A(x=10, y=20)) == id(A(x=10, y=20)))
print(A(x=10, y=20) == B(x=10, y=20))
print(A(x=10, y=20) == C(i=10, j=20))

print(A(x=10, y=20) < A(x=20, y=10))
