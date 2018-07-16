import urllib
import pkgutil

module = urllib
print(dir(module))
# ['__builtins__', '__cached__', '__doc__', '__file__', '__loader__', '__name__', '__module__', '__path__', '__spec__']

for submodule in pkgutil.walk_packages(module.__path__):
    print(submodule.name)
# error
# parse
# request
# response
# robotparser
