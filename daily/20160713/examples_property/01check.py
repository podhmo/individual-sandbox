class A:
    @property
    def x(self):
        return "property"

print(A().x)
a = A()
try:
    a.x = "value"
except AttributeError as e:
    print(e)

print("-")


class B:
    @property
    def x(self):
        return getattr(self, "_x", None) or "property"

    @x.setter
    def x(self, v):
        self._x = v

b = B()
print(b.x)
b.x = "value"
print(b.x)
