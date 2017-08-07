def powerset(xs):
    if not xs:
        return [[]]
    else:
        subset = powerset(xs[1:])
        return [*subset, *[[xs[0], *ys] for ys in subset]]
