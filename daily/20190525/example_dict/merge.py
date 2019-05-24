import operator


def _to_accesssor(k):
    if callable(k):
        return k
    elif isinstance(k, (str, bytes)):
        return operator.itemgetter(k)
    elif isinstance(k, (list, tuple)):
        return lambda v: tuple([v.get(sk) for sk in k])
    else:
        raise ValueError(k)


class Merger:
    def __init__(
        self, *, missing_value=None, fill=True, accessor_factory=_to_accesssor
    ):
        self.missing_value = missing_value
        self.fill = fill
        self.accessor_factory = accessor_factory

    def merge(self, left, right, *, how="inner", left_on=None, right_on=None, on=None):
        assert on or (left_on and right_on)

        if on is not None:
            left_on = right_on = on
        left_on = self.accessor_factory(left_on)
        right_on = self.accessor_factory(right_on)

        if how == "inner":
            return self._merge_inner(left, right, left_on, right_on)
        elif how == "left":
            return self._merge_left(left, right, left_on, right_on)
        elif how == "right":
            return self._merge_left(right, left, right_on, left_on)
        elif how == "outer":
            return self._merge_outer(left, right, left_on, right_on)
        else:
            raise ValueError("invalid merge method {}".format(how))

    __call__ = merge

    def _merge_left(self, left, right, left_k, right_k):
        right_cache = {right_k(x): x for x in right}
        if self.fill:
            keys = set()
            for rv in right:
                keys.update(rv.keys())
            keys = tuple(keys)

        r = []
        unfilled = []
        for lv in left:
            k = left_k(lv)
            d = lv.copy()
            if k in right_cache:
                d.update(right_cache[k])
            else:
                unfilled.append(d)
            r.append(d)
        if self.fill:
            for d in unfilled:
                for k in keys:
                    if k not in d:
                        d[k] = self.missing_value
        return r

    def _merge_outer(self, left, right, left_k, right_k):
        large_k, small_k, large, small = _ordered(left, right, left_k, right_k)
        small_cache = {small_k(x): x for x in small}

        r = []
        unfilled = []
        small_used = set()
        if self.fill:
            small_keys = set()
            large_keys = set()
        else:
            large_keys = small_keys = None

        for lv in large:
            lk = large_k(lv)
            d = lv.copy()
            if lk in small_cache:
                sv = small_cache[lk]
                d.update(sv)
                small_used.add(lk)
                if small_keys is not None:
                    small_keys.update(sv.keys())
            else:
                unfilled.append(d)
            if large_keys is not None:
                large_keys.update(lv.keys())
            r.append(d)
        if self.fill:
            small_keys = tuple(small_keys)
            for x in unfilled:
                for k in small_keys:
                    if k not in x:
                        x[k] = self.missing_value
            large_defaults = {k: self.missing_value for k in large_keys}
        else:
            large_defaults = None

        for sv in small:
            sk = small_k(sv)
            if sk in small_used:
                continue
            d = sv.copy()
            if large_defaults is not None:
                for k, v in large_defaults.items():
                    if k not in d:
                        d[k] = v
            r.append(d)
        return r

    def _merge_inner(self, left, right, left_k, right_k):
        large_k, small_k, large, small = _ordered(left, right, left_k, right_k)
        small_cache = {small_k(sv): sv for sv in small}
        r = []
        for lv in large:
            lk = large_k(lv)
            if lk not in small_cache:
                continue
            d = lv.copy()
            d.update(small_cache[lk])
            r.append(d)
        return r


def _ordered(left, right, left_k, right_k):
    if len(left) > len(right):
        return left_k, right_k, left, right
    else:
        return right_k, left_k, right, left


merge = Merger()  # noqa


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
