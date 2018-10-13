import itertools


def _all_keys(xs, ys):
    if not xs:
        return ys
    if not ys:
        return xs

    seen = set(xs)
    pre = []
    r = [[x] for x in xs]
    i = 0
    first = True
    for y in ys:
        if y in seen:
            first = False
            while r[i][0] != y:
                i += 1
        else:
            if first:
                pre.append(y)
            else:
                r[i].append(y)
    return list(itertools.chain(pre, *r))


print(_all_keys([1, 2, 3], [1, 2, 3, 4]))
print(_all_keys([1, 2, 3], [0, 1, 2, 3, 4]))
print(_all_keys([0, 1, 2, 3], [1, 2, 3, 4]))
print(_all_keys([1, 0, 2, 3], [1, 2, 3, 4]))
print(_all_keys([1, 0, -1, 2, 3], [1, 2, 3, 4]))
print(_all_keys([0, 1, 2, 3, 5], [1, 2, 3, 4]))
