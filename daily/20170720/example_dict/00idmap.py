from collections import defaultdict

idmap = defaultdict(lambda: len(idmap))
xs = ["x", "y", "z", "x", "x", "a"]

for x in xs:
    print(x, idmap[x])
