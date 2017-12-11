import inspect


def f(x, y, *, z, **kwargs):
    return x + 1


def g(x, y, err=None, status=None):
    pass


def h(x, y, status=None, err=None):
    pass


print(inspect.signature(f))
print(inspect.getfullargspec(f))

print("----------------------------------------")
print(inspect.signature(g))
print(inspect.signature(h))
print(inspect.getfullargspec(g))
print("========================================")


class Ob:
    def add(self, x, y):
        return x + y


def add(x, y):
    pass


sig = inspect.signature(Ob.add)
# if "self" in sig.parameters:
#     del sig.parameters["self"]
print(inspect.signature(add) == sig)
print(str(inspect.signature(add)), str(sig).replace('(self, ', '('))
print(str(inspect.signature(add)) == str(sig).replace('(self, ', '('))
