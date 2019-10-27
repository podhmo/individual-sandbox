import unittest
from magicalimport import import_symbol

glob = import_symbol("./08walk.py:glob")

d = {"a": {"b": {"hello.py": None}}}
print(sorted(glob(d, "a/b/hello.py")))
print(sorted(glob(d, "**/hello.py")))
print(sorted(glob(d, "**/*.py")))
