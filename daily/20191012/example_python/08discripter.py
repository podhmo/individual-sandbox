class field:
    def __init__(self, default, *, metadata=None):
        self.default = default
        self.metadata = metadata

    def __get__(self, obj, type=None):
        return self.default


class A:
    x = field(10)


print(A.__dict__["x"].__dict__)
print(A.x)
print(A().x)
