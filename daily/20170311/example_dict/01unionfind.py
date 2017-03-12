import json
from collections import defaultdict


class UnionFind(object):
    def __init__(self):
        self.world = {}
        self.rank = defaultdict(int)

    def unite(self, x, y):
        return unite(self.world, self.rank, x, y)

    def index(self, k):
        return index(self.world, k)

    def is_same(self, x, y):
        return self.index(x) == self.index(y)


def index(world, k):
    if (world.get(k) or k) == k:
        return k
    else:
        v = world[k] = index(world, world.get(k) or k)
        return v


def unite(world, rank, x, y):
    ix = index(world, x)
    iy = index(world, y)
    if ix == iy:
        return
    if rank[ix] < rank[iy]:
        world[ix] = iy
    else:
        world[iy] = ix
        if rank[ix] == rank[iy]:
            rank[ix] += 1


def classify(uf, region):
    d = defaultdict(list)
    for i in region:
        d[uf.index(i)].append(i)
    return d


uf = UnionFind()

print("start")
print("\t", json.dumps(classify(uf, range(1, 10)), sort_keys=True))
for x, y in [(3, 4), (4, 9), (8, 0), (2, 3), (5, 6), (5, 9), (7, 3), (4, 8)]:
    print("unite({}, {})".format(x, y))
    uf.unite(x, y)
    print("\t", json.dumps(classify(uf, range(1, 10)), sort_keys=True))
