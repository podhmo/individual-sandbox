from namedlist import namedlist

A = namedlist("A", "x y")
B = namedlist("B", "x y")
C = namedlist("C", "i j")

print(A(x=10, y=20) == A(x=10, y=20))
print(A(x=10, y=20) == B(x=10, y=20))
print(A(x=10, y=20) == C(i=10, j=20))

print(A(x=10, y=20) < A(x=20, y=10))
