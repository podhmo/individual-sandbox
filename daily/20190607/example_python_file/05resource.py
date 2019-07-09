from importlib.util import find_spec

spec = find_spec("lib2to3")
spec.loader.path  # => '/usr/lib/python3.7/lib2to3/__init__.py'
spec.loader.resource_path("Grammar.txt")  # => '/usr/lib/python3.7/lib2to3/Grammar.txt'
import importlib.resources

with importlib.resources.path("lib2to3", "Grammar.txt") as p:
    print(p)
