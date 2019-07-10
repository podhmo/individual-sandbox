def f(x):
    return x + 1


def f2(x):
    def g(x):
        return x * x

    return g(x)
