import json
from collections import defaultdict, namedtuple

s = """
{
  "name": "boo",
  "age": 0,
  "parents": [
    {
      "name": "foo",
      "age": 20,
      "skills": [{"name": "A"}, {"name": "B"}]
    },
    {
      "name": "bar",
      "age": 20,
      "skills": [{"name": "A"}, {"name": "C"}]
    }
  ]
}
"""

idmap = defaultdict(lambda: str(len(idmap)))


def on_pairs(pairs, named={}):
    fields = tuple(sorted([p[0].lstrip("_") for p in pairs]))
    k = idmap[fields]
    if k not in named:
        named[k] = namedtuple("N" + k, " ".join(fields))
    return named[k](*[p[1] for p in pairs])


print(json.loads(s, object_pairs_hook=on_pairs))
