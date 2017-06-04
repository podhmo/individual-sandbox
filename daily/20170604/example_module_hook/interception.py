import sys


def hookwrap(fn):
    if getattr(fn, "wrapped", False):
        return fn

    def wrap(*args, **kwargs):
        print("@@", args, kwargs, file=sys.stderr)
        return fn(*args, **kwargs)

    wrap.wrapped = True
    return wrap


def setup():
    for i in range(len(sys.path_hooks)):
        sys.path_hooks[i] = hookwrap(sys.path_hooks[i])
