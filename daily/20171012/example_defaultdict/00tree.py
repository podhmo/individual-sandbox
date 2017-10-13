import json
from collections import defaultdict


def tree():
    return defaultdict(tree)


t = tree()
t["a"]["b"]["c"] = 1
t["a"]["b"]["d"] = 2
print(json.dumps(t, indent=2))
