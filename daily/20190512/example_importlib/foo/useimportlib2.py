from importlib import import_module
me = import_module(".me", package="foo")
print(me.me)
