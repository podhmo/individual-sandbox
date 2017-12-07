def f(x):
    return g(x)  # error


def g(x, y):
    return g + y
