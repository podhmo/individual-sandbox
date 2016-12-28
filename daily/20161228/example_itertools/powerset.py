def powerset(xs):
    if not xs:
        return [[]]
    else:
        return [[*y, *zs] for zs in powerset(xs[1:]) for y in [[], [xs[0]]]]

print(powerset([1, 2, 3]))
