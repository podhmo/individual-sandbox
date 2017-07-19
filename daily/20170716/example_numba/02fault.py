from numba import jit, int32


@jit(nopython=True)
def f(x, y):
    return x + y


@jit(int32(int32, int32), nopython=True)
def g(x, y):
    return x + y


print(f(10, 10))
print(g(10, 10))
print(f(100000000000000000, 100000000000000000))
print(g(100000000000000000, 100000000000000000))
print(f("foo", "bar"))
