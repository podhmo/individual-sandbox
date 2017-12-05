from functools import partial
from collections import defaultdict


class Context:
    def __init__(self):
        self.values = []
        self.counter = defaultdict(int)
        self.i = 0

    def add(self, x):
        if x not in self.counter:
            self.values.append(x)
        self.counter[x] += 1

    def __iter__(self):
        return iter(self.values)


class Shaper:
    def __init__(self, iterate=partial(sorted, key=str)):
        self.iterate = iterate

    def shape(self, d):
        r = Context()
        self._shape(d, r, [])
        return r

    def _shape(self, d, r, path):
        if hasattr(d, "keys"):
            self._shape_dict(d, r, path)
        elif isinstance(d, (list, tuple)):
            self._shape_list(d, r, path)
        else:
            self._shape_atom(d, r, path)

    def _shape_dict(self, d, r, path):
        self._emit(d, r, path)
        for k in self.iterate(d.keys()):
            path.append(k)
            self._shape(d[k], r, path)
            path.pop()

    def _shape_list(self, xs, r, path):
        path.append("[]")
        self._emit(xs, r, path)
        for x in xs:
            self._shape(x, r, path)
        path.pop()

    def _shape_atom(self, v, r, path):
        self._emit(v, r, path)

    def _emit(self, v, r, path):
        r.add(tuple(path[:]))


def visualize(ctx):
    r = []
    for x in ctx.values:
        if not x:
            continue
        if ctx.counter[x[:-1]] - ctx.counter[x] <= 1:
            r.append(".".join(x))
        else:
            r.append("?{}".format(".".join(x)))
    return r


def shape(d, shaper=Shaper(), visualize=visualize):
    return visualize(shaper.shape(d))
