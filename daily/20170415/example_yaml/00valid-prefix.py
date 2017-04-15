import yaml
from io import StringIO


def loads(s):
    return yaml.load(StringIO(s))

# ok
print(loads("$foo:1"))
print(loads(":foo:1"))
print(loads("-foo:1"))
print(loads("~foo:1"))
print(loads("+foo:1"))


# error
# print(loads("@foo:1"))
# print(loads("%foo:1"))
# yaml.scanner.ScannerError: while scanning for the next token
# found character '@' that cannot start any token
#   in "<file>", line 1, column 1
