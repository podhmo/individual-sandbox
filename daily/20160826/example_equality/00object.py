class A(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y


class B(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y


class C(object):
    def __init__(self, i, j):
        self.i = i
        self.j = j


print(A(x=10, y=20) == A(x=10, y=20))
print(A(x=10, y=20) == B(x=10, y=20))
print(A(x=10, y=20) == C(i=10, j=20))

