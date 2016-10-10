import yaml
from io import StringIO


def loads(s):
    io = StringIO(s)
    return yaml.load(io)


s = """\
foo:
  !!pairs
  - a: b
  - x: y
bar:
  - a: b
  - x: y
"""

print(loads(s))
