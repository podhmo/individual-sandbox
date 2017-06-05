import sys
defaults = list(sys.modules.keys())
from magicalimport import import_from_physical_path  # NOQA
d = import_from_physical_path("../../20170604/example_module_hook/deps2.py")  # NOQA
for k in list(sys.modules.keys()):
    if k not in defaults:
        del sys.modules[k]
dag = d.Dag()  # NOQA
d.setup(hook=dag.add)  # NOQA
factory = d.HookedLoaderFactory(dag.add)
for finder in sys.path_importer_cache.values():
    if finder and hasattr(finder, "_loaders"):
        finder._loaders = factory.new_loaders(finder._loaders)
from wsgiref.simple_server import make_server  # NOQA
print(dag.to_dot())
