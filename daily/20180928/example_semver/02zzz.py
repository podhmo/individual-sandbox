from semver import max_satisfying, make_semver, sort
from collections import defaultdict


def custom_max_satisfying(candidates, query):
    if not query.startswith("~"):
        return max_satisfying(candidates, query, loose=True)
    else:
        normalized = defaultdict(list)
        for x in candidates:
            v = make_semver(x, loose=True)
            normalized["{}.{}.{}".format(v.major, v.minor, v.patch)].append(v)
        mv = max_satisfying(list(normalized), query)
        return sort(normalized[mv], loose=True)[-1].version



print(custom_max_satisfying(["1.2.3-pre"], "~1.2.3"))
# 1.2.3-pre
print(custom_max_satisfying(["1.2.3", "1.2.3-pre"], "~1.2.3"))
# 1.2.3
print(custom_max_satisfying(["1.2.3", "1.2.3-pre", "1.2.4-pre"], "~1.2.3"))
# 1.2.4-pre
