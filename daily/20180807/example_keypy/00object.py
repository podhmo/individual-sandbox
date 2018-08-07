from pprint import pprint
from collections import defaultdict


class P:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __repr__(self):
        return f"<{id(self)} x={self.x}, y={self.y}>"

    def __hash__(self):
        return hash((self.x, self.y))

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y


p0 = P(100, 200)
p1 = P(100, 200)
p2 = P(100, -200)
L = [p0, p1, p2]

d = defaultdict(list)
for p in L:
    d[p].append(p)
pprint(d)
