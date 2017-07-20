import json
from collections import defaultdict, namedtuple
from pympler.asizeof import asizeof

idmap = defaultdict(lambda: str(len(idmap)))


def on_pairs(pairs, named={}):
    fields = tuple(sorted([p[0].lstrip("_") for p in pairs]))
    k = idmap[fields]
    if k not in named:
        named[k] = namedtuple("N" + k, " ".join(fields))
    return named[k](*[p[1] for p in pairs])


with open("./data.json") as rf:
    d0 = json.load(rf)

with open("./data.json") as rf:
    d1 = json.load(rf, object_pairs_hook=on_pairs)

print(type(d0), asizeof(d0))  # <class 'list'> 54856
print(type(d1), asizeof(d1))  # <class 'list'> 30528
# for k, v in idmap.items():
#     print(k, v)
