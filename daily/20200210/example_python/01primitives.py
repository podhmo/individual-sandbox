import sys
import inspect

for val in sys.modules["builtins"].__dict__.values():
    if not inspect.isclass(val):
        continue
    if issubclass(val, BaseException):
        continue
    print(val.__qualname__)
