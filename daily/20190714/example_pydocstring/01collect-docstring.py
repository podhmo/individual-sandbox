import sys
import subprocess
import importlib
import inspect
from dictknife import Accessor
from dictknife import loading
from handofcats import as_command


@as_command
def run(pkgname: str) -> None:
    p = subprocess.run(
        ["pyinspect", "list", pkgname], text=True, stdout=subprocess.PIPE
    )
    a = Accessor()
    r = a.make_dict()
    for module_name in p.stdout.split():
        try:
            m = importlib.import_module(module_name)
        except Exception as e:
            print(f"\x1b[32m!! {e!r} \x1b[0m", file=sys.stderr)
        for name, val in m.__dict__.items():
            if inspect.isclass(val):
                a.assign(r, [module_name, name], inspect.getdoc(val))
            elif inspect.isfunction(val):
                a.assign(r, [module_name, name], inspect.getdoc(val))
    loading.dumpfile(r)
