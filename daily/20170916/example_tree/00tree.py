from collections import OrderedDict


class Tree:
    def __init__(self):
        self.children = OrderedDict()
        self.leafs = set()

    def __contains__(self, k):
        return k in self.children

    def subtree(self, k):
        if k not in self:
            self.children[k] = self.__class__()
        return self.children[k]

    def add_leaf(self, v):
        self.leafs.add(v)


def dump_tree(t, indent=""):
    for k, st in t.children.items():
        print("{}{}".format(indent, k))
        dump_tree(st, indent=indent + "  ")
    for v in t.leafs:
        print("{}{}".format(indent, v))


def build_tree(L):
    t = Tree()

    for ks in L:
        cursor = t
        for k in ks[:-1]:
            cursor = cursor.subtree(k)
        cursor.add_leaf(ks[-1])
    return t


def omit_access(d, t, *, hist=None):
    hist = hist or []
    for k in d.keys():
        if k in t:
            hist.append(k)
            yield from omit_access(d[k], t.children[k], hist=hist)
            hist.pop()
        elif k in t.leafs:
            continue
        else:
            hist.append(k)
            yield hist[:], d[k]
            hist.pop()


d = {
    "a": 1,
    "b": 2,
    "x": {
        "y0": {
            "z00": 10,
            "z01": 11
        },
        "y1": {
            "z10": 100,
            "z11": 101
        }
    },
}

t = build_tree([["a"]])
t = build_tree([["a"], ["x"]])
t = build_tree([["a"], ["x", "y1"]])
dump_tree(t)
print(list(omit_access(d, t)))

