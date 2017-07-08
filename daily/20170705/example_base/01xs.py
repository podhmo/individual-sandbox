def permutations(xs, n):
    if n <= 0:
        return []
    elif n == 1:
        return [[x] for x in xs]
    else:
        return [
            [x, *ys] for i, x in enumerate(xs)
            for ys in permutations([*xs[:i], *xs[i + 1:]], n - 1)
        ]


def combinations(xs, n):
    if n <= 0:
        return []
    elif n == 1:
        return [[x] for x in xs]
    else:
        return [[xs[i], *ys] for i in range(0, len(xs)) for ys in combinations(xs[i + 1:], n - 1)]


xs = ["a", "b", "c", "d", "e", "f", "g"]
import itertools as it  # NOQA
print(
    all(
        sorted(list(xs) for xs in permutations(xs, i)) ==
        sorted(list(xs) for xs in it.permutations(xs, i)) for i in range(1, 6)
    )
)
print(
    all(
        sorted(list(xs) for xs in combinations(xs, i)) ==
        sorted(list(xs) for xs in it.combinations(xs, i)) for i in range(1, 6)
    )
)
