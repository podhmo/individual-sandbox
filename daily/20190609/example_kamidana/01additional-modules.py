import os.path
import inspect
from importlib import resources, import_module
from collections import OrderedDict
from dictknife import loading


contents = list(resources.contents("kamidana.additionals"))
candidates = OrderedDict()
for filename in contents:
    if not filename.endswith(".py"):
        continue
    if filename == "__init__.py":
        continue
    modulename = f"kamidana.additionals.{os.path.splitext(filename)[0]}"
    candidates[modulename] = inspect.getdoc(import_module(modulename))
loading.dumpfile(candidates, format="json")
