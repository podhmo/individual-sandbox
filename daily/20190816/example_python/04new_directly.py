class A:
    def __init__(self, x):
        self.x = x


print(A.__new__)
print(A.__new__(A))
print(getattr(A.__new__(A), "x", None))
a = A.__new__(A)
b = A("x")
a.__dict__.update(b.__dict__)
print(getattr(a, "x", None))
