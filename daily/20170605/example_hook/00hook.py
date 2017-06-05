import sys
defaults = list(sys.modules.keys())
from magicalimport import import_from_physical_path  # NOQA
d = import_from_physical_path("../../20170604/example_module_hook/deps2.py")  # NOQA
for k in list(sys.modules.keys()):
    if k not in defaults:
        del sys.modules[k]
d.setup()  # NOQA
from wsgiref.simple_server import make_server  # NOQA
