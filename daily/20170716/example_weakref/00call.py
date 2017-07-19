import weakref


def hello(*args, **kwargs):
    print("deleted", args, kwargs)


A = type("A", (), {})
a = A()

ref = weakref.ref(a, hello)
print(ref())
del a
print(ref())
