from collections import OrderedDict, defaultdict
import itertools

it = iter([2, 1, 3, 2])
it0, it1 = itertools.tee(it, 2)
d = OrderedDict.fromkeys(it0)
c = defaultdict(int)
for x in it1:
    c[x] += 1
d.update(c)
print(d)
