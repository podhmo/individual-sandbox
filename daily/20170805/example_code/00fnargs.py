import inspect


def fn(x, y):
    pass


print(inspect.getargspec(fn))
print(inspect.signature(fn))
print(dir(fn))
print(dir(fn.__code__))
print(fn.__signature__)
