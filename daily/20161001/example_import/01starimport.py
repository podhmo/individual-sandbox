from importlib import machinery
import sys


def expose_all_members(module, globals_=None, _depth=2):
    members = {k: v for k, v in module.__dict__.items() if not k.startswith("_")}
    return expose_members(module, members, globals_=globals_, _depth=_depth)


def expose_members(module, members, globals_=None, _depth=1):
    if globals_ is None:
        frame = sys._getframe(_depth)
        globals_ = frame.f_globals
    globals_.update({k: module.__dict__[k] for k in members})
    return globals_


def import_by_physical_path(path, as_=None):
    module_id = as_ or path.replace("/", "_").rstrip(".py")
    return machinery.SourceFileLoader(module_id, path).load_module()


# import foo as foo2
foo2 = import_by_physical_path("./foo.py", as_="foo2")
print("name:{}".format(foo2.name))
print("age:{}".format(foo2._age))


# from foo import * (but _age is not imported)
expose_all_members(import_by_physical_path("./foo.py"))
print("name:{}".format(name))  # NOQA
try:
    print(_age)
except NameError as e:
    print("\tE: {}".format(e))


# from foo import _age
expose_members(import_by_physical_path("./foo.py"), members=["_age"])
print("_age:", _age)  # NOQA
