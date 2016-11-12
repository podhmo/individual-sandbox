from collections import Counter


def lower_key_dict(d):
    for k in d.keys():
        d[k.lower()] = d.pop(k)
    return d


c = Counter()
for _ in range(1000):
    k = tuple(lower_key_dict({"A": "1", "B": "2"}).keys())
    c[k] += 1
print(c)
