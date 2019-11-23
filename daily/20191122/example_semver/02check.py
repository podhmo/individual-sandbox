from functools import cmp_to_key
from semver import make_semver


# helpers
def _sorted(versions, loose=False, key=None):
    key = key or cmp_to_key(lambda x, y: x.compare(y))
    return sorted([make_semver(v, loose=loose) for v in versions], key=key)


v1 = "1.1"
v2 = "1.1.1"
v3 = "1.1.1-pre1"
v4 = "1.1.1.1"
v5 = "1.1.1.2"

print(_sorted([v1, v2, v3, v4, v5], loose=True))
print(_sorted(reversed([v1, v2, v3, v4, v5]), loose=True))
print([v.raw for v in _sorted(reversed([v1, v2, v3, v4, v5]), loose=True)])
