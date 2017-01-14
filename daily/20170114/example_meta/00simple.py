class A:
    def __init__(self, x=1, y=2, z=3):
        self.x = x
        self.y = y
        self.z = z

    def __repr__(self):
        return "{self.__class__.__name__}(x={self.x!r}, y={self.y!r}, z={self.z!r})".format(self=self)

print(A())  # A(x=1, y=2, z=3)
print(A(x=10, y=20))  # A(x=10, y=20, z=3)
