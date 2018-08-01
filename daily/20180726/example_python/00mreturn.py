def f():
    return 1, 2


def g():
    return 1, 2, 3


def h():
    return 1, 2, 3, 4


r = []
for fn in [f, g, h]:
    x, y, *rest = fn()
    r.append((x, y, rest))
print(r)
# [(1, 2, []), (1, 2, [3]), (1, 2, [3, 4])]
