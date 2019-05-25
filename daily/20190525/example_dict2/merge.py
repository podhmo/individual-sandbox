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


class Options:
    def __init__(self, *, missing_value=None, accessor_factory=_to_accesssor) -> None:
        self.missing_value = missing_value
        self.accessor_factory = accessor_factory


_default_options = Options()


def how_inner_join(left, right, left_k, right_k, *, options=_default_options):
    large_k, small_k, large, small, create = _ordered(left, right, left_k, right_k)
    small_cache = defaultdict(list)
    for x in small:
        small_cache[small_k(x)].append(x)

    for lv in large:
        lk = large_k(lv)
        if lk not in small_cache:
            continue
        for sv in small_cache[lk]:
            yield create(small=sv, large=lv)


def how_left_outer_join(left, right, left_k, right_k, *, options=_default_options):
    missing_value = options.missing_value

    right_cache = defaultdict(list)
    for x in right:
        right_cache[right_k(x)].append(x)

    for lv in left:
        k = left_k(lv)
        if k in right_cache:
            for rv in right_cache[k]:
                yield (lv, rv)
        else:
            yield (lv, missing_value)


def how_right_outer_join(left, right, left_k, right_k, *, options=_default_options):
    return how_left_outer_join(right, left, right_k, left_k, options=options)


def how_full_outer_join(left, right, left_k, right_k, *, options=_default_options):
    missing_value = options.missing_value

    large_k, small_k, large, small, create = _ordered(left, right, left_k, right_k)
    small_cache = defaultdict(list)
    for x in small:
        small_cache[small_k(x)].append(x)

    small_used = set()

    for lv in large:
        lk = large_k(lv)
        if lk in small_cache:
            small_used.add(lk)
            for sv in small_cache[lk]:
                yield create(small=sv, large=lv)
        else:
            yield create(small=missing_value, large=lv)

    for sv in small:
        sk = small_k(sv)
        if sk in small_used:
            continue
        yield create(small=sv, large=missing_value)


def _ordered(left, right, left_k, right_k):
    # xxx: len(), so cannot passing Iterator!!
    if len(left) > len(right):
        return left_k, right_k, left, right, lambda *, small, large: (large, small)
    else:
        return right_k, left_k, right, left, lambda *, small, large: (small, large)


def merge(
    left,
    right,
    *,
    left_on=None,
    right_on=None,
    on=None,
    how=how_inner_join,
    options=_default_options,
):
    assert on or (left_on and right_on)

    if on is not None:
        left_on = right_on = on

    left_on_accessor = options.accessor_factory(left_on)
    right_on_accessor = options.accessor_factory(right_on)
    yield from how(left, right, left_on_accessor, right_on_accessor, options=options)
