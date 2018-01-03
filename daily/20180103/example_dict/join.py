import operator


def to_accesssor(k):
    if callable(k):
        return k
    elif isinstance(k, (str, bytes)):
        return operator.itemgetter(k)
    elif isinstance(k, (list, tuple)):
        return lambda v: tuple([v.get(sk) for sk in k])
    else:
        raise ValueError(k)


def innerjoin(left, right, *, on=None, left_on=None, right_on=None, suffixes=("", "")):
    assert on or (left_on and right_on)

    if on is not None:
        left_on = right_on = on
    left_on = to_accesssor(left_on)
    right_on = to_accesssor(right_on)

    if len(left) < len(right):
        small, large, getsk, getlk, ssuffix, lsuffix = left, right, left_on, right_on, suffixes[
            0
        ], suffixes[1]
    else:
        small, large, getsk, getlk, ssuffix, lsuffix = right, left, right_on, left_on, suffixes[
            1
        ], suffixes[0]

    scache = {getsk(sv): sv for sv in small}

    r = []
    for lv in large:
        k = getlk(lv)
        d = {lk + lsuffix: v for lk, v in lv.items()}
        if k in scache:
            for sk, v in scache[k].items():
                d[sk + ssuffix] = v
        r.append(d)
    return r
