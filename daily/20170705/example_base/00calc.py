xs = [1, 10, 100]

# ans = [11,101,110,111]


def powerset(xs):
    if not xs:
        return []
    elif len(xs) == 1:
        return [xs]
    else:
        return [[*ys, *zs] for zs in powerset(xs[1:]) for ys in [[], [xs[0]]]]

print({sum(ns) for ns in powerset(xs) if len(ns) >= 2})
