import json
from collections import defaultdict, namedtuple
from pympler.asizeof import asizeof
from tracemalloc import _format_size

idmap = defaultdict(lambda: str(len(idmap)))


def on_pairs(pairs, named={}):
    fields = tuple(sorted([p[0].lstrip("_") for p in pairs]))
    k = idmap[fields]
    if k not in named:
        named[k] = namedtuple("N" + k, " ".join(fields))
    return named[k](*[p[1] for p in pairs])


filename = "citylots.json"  # size is 181 Mib
with open(filename) as rf:
    d0 = json.load(rf, object_pairs_hook=on_pairs)

print(len(idmap))
print(type(d0), _format_size(asizeof(d0), False))
# 4
# <class '__main__.N3'> 638 MiB

# real    1m45.924s
# user    1m38.756s
# sys     0m5.089
