import inspect


def f(x: int, verbose: bool = False, *, y: int, z: int = 0, **kwargs):
    pass


spec = inspect.getfullargspec(f)
for name in dir(spec):
    if name.startswith("_"):
        continue
    print("@@", name, getattr(spec, name))
"""
@@ annotations {'x': <class 'int'>, 'verbose': <class 'bool'>, 'y': <class 'int'>, 'z': <class 'int'>}
@@ args ['x', 'verbose']
@@ count <built-in method count of FullArgSpec object at 0x7faf3f22b9e8>
@@ defaults (False,)
@@ index <built-in method index of FullArgSpec object at 0x7faf3f22b9e8>
@@ kwonlyargs ['y', 'z']
@@ kwonlydefaults {'z': 0}
@@ varargs None
@@ varkw kwargs
"""
