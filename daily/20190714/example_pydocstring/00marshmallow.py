import subprocess
import importlib
import inspect
from dictknife import Accessor
from dictknife import loading

p = subprocess.run(
    ["pyinspect", "list", "marshmallow"], text=True, stdout=subprocess.PIPE
)
a = Accessor()
r = a.make_dict()
for module_name in p.stdout.split():
    m = importlib.import_module(module_name)
    for name, val in m.__dict__.items():
        if inspect.isclass(val):
            a.assign(r, [module_name, name], inspect.getdoc(val))
        elif inspect.isfunction(val):
            a.assign(r, [module_name, name], inspect.getdoc(val))
loading.dumpfile(r)
