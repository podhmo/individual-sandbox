def f(*xs, prefix="no."):
    return [f"{prefix} {x}" for x in xs]


print(f(1, 2, 3))

