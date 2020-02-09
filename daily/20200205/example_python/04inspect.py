import inspect


def f(x: int, *, y: int, z: int):
    pass


v = inspect.getcallargs(f, 1, y=1, z=1)
print(v)
print(inspect.signature(f))
