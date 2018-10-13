import itertools


def combinations(xs, n):
    if n < 1:
        return ValueError("invalid,  must be n > 0")
    elif n == 1:
        return [(x, ) for x in xs]
    else:
        return [(xs[i], *ys) for i in range(len(xs) - 1) for ys in combinations(xs[i + 1:], n - 1)]


print(combinations([1, 2, 3, 4, 5], 1))
print(combinations([1, 2, 3, 4, 5], 2))
print(combinations([1, 2, 3, 4, 5], 3))
print(combinations([1, 2, 3, 4, 5], 4))
print(combinations([1, 2, 3, 4, 5], 5))
print(combinations([1, 2, 3, 4, 5], 6))
assert sorted(list(combinations([1, 2, 3, 4, 5],
                                2))) == sorted(list(itertools.combinations([1, 2, 3, 4, 5], 2)))
assert sorted(list(combinations([1, 2, 3, 4, 5],
                                3))) == sorted(list(itertools.combinations([1, 2, 3, 4, 5], 3)))
