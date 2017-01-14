# with meta
class B:
    def __init__(self, x=None, y=None, z=None):
        self.x = x if x is not None else self.Meta.x
        self.y = y if y is not None else self.Meta.y
        self.z = z if z is not None else self.Meta.z

    def __repr__(self):
        return "{self.__class__.__name__}(x={self.x!r}, y={self.y!r}, z={self.z!r})".format(self=self)

    class Meta:
        x = 1
        y = 2
        z = 3


print(B())  # B(x=1, y=2, z=3)
print(B(y=20))  # B(x=1, y=20, z=3)


class C(B):
    class Meta:
        x = 10
        y = 20

# hmm
# print(C())
# AttributeError: type object 'Meta' has no attribute 'z'

print(C(z=3))  # C(x=10, y=20, z=3)
