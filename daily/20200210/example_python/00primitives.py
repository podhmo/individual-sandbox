import sys

for val in sys.modules["builtins"].__dict__.values():
    if not isinstance(val, type):
        continue
    print(val.__qualname__)

