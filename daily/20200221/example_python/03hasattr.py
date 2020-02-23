class A:
    def __getattr__(self, name):
        return A()


A().x.x.x.x.x
print(hasattr(A(), "x"))
print(getattr(A(), "x"))
