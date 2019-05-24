import operator
from collections import defaultdict


def _to_accesssor(k):
    if callable(k):
        return k
    elif isinstance(k, (str, bytes)):
        return operator.itemgetter(k)
    elif isinstance(k, (list, tuple)):
        # todo: compile?
        return lambda v: tuple([v.get(sk) for sk in k])
    else:
        raise ValueError(k)


def how_left_outer_join(
    left, right, left_k, right_k, *, fill=True, missing_value=None
):
    right_cache = defaultdict(list)
    for x in right:
        right_cache[right_k(x)].append(x)

    if fill:
        keys = set()
        for rv in right:
            keys.update(rv.keys())
        keys = tuple(keys)

    r = []
    unfilled = []
    for lv in left:
        k = left_k(lv)
        if k in right_cache:
            for rv in right_cache[k]:
                d = lv.copy()
                d.update(rv)
                r.append(d)
        else:
            d = lv.copy()
            unfilled.append(d)
            r.append(d)
    if fill:
        for d in unfilled:
            for k in keys:
                if k not in d:
                    d[k] = missing_value
    return r


def how_right_outer_join(
    left, right, left_k, right_k, *, fill=True, missing_value=None
):
    return how_left_outer_join(
        right, left, right_k, left_k, fill=fill, missing_value=missing_value
    )


def how_full_outer_join(
    left, right, left_k, right_k, *, fill=True, missing_value=None
):
    large_k, small_k, large, small = _ordered(left, right, left_k, right_k)
    small_cache = defaultdict(list)
    for x in small:
        small_cache[small_k(x)].append(x)

    r = []
    unfilled = []
    small_used = set()

    large_keys = small_keys = None
    if fill:
        small_keys = set()
        large_keys = set()

    for lv in large:
        lk = large_k(lv)
        if lk in small_cache:
            small_used.add(lk)
            for sv in small_cache[lk]:
                d = lv.copy()
                d.update(sv)
                r.append(d)
                if fill:
                    small_keys.update(sv.keys())
        else:
            d = lv.copy()
            unfilled.append(d)
            r.append(d)
        if fill:
            large_keys.update(lv.keys())

    large_defaults = None
    if fill:
        small_keys = tuple(small_keys)
        for x in unfilled:
            for k in small_keys:
                if k not in x:
                    x[k] = missing_value
        large_defaults = {k: missing_value for k in large_keys}

    for sv in small:
        sk = small_k(sv)
        if sk in small_used:
            continue
        d = sv.copy()
        if fill:
            for k, v in large_defaults.items():
                if k not in d:
                    d[k] = v
        r.append(d)
    return r


def how_inner_join(left, right, left_k, right_k, *, fill=True, missing_value=None):
    large_k, small_k, large, small = _ordered(left, right, left_k, right_k)
    small_cache = defaultdict(list)
    for x in small:
        small_cache[small_k(x)].append(x)

    r = []
    for lv in large:
        lk = large_k(lv)
        if lk not in small_cache:
            continue
        for rv in small_cache[lk]:
            d = lv.copy()
            d.update(rv)
            r.append(d)
    return r


def _ordered(left, right, left_k, right_k):
    if len(left) > len(right):
        return left_k, right_k, left, right
    else:
        return right_k, left_k, right, left


def merge(
    left,
    right,
    *,
    left_on=None,
    right_on=None,
    on=None,
    how=how_inner_join,
    accessor_factory=_to_accesssor,
    missing_value=None,
    fill=True,
):
    assert on or (left_on and right_on)

    if on is not None:
        left_on = right_on = on
    left_on = accessor_factory(left_on)
    right_on = accessor_factory(right_on)
    return how(left, right, left_on, right_on, fill=fill, missing_value=missing_value)


# include/exclude? or only/ignore?
def with_prefix(prefix, d, *, exclude=None, mutable=False):
    exclude = exclude or []
    _with_prefix = _with_prefix_mutable if mutable else _with_prefix_immutable
    if hasattr(d, "append"):
        return [_with_prefix(prefix, x, exclude=exclude) for x in d]
    else:
        return _with_prefix(prefix, d, exclude=exclude)


def _with_prefix_immutable(prefix, d, *, exclude):
    return {(k if k in exclude else prefix + k): v for k, v in d.items()}


def _with_prefix_mutable(prefix, d, *, exclude):
    for k in list(d.keys()):
        if k not in exclude:
            d[prefix + k] = d.pop(k)
    return d
