import inspect

class A:
    class B:
        pass


class C(A):
    class D:
        pass


print(C.B.__qualname__)
print(C.D.__qualname__)
print(C.__dict__["D"])

for name, val in inspect.getmembers(C):
    if inspect.isclass(val) and val.__qualname__.startswith(C.__name__):
        print(name, val)
