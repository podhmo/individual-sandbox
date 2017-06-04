import sys
import inspect
import traceback


def normalize(name):
    for path in sys.path:
        name = name.replace(path, "")
    return name.lstrip("/").rsplit("/__init__.py", 1)[0].rsplit(".py", 1)[0].replace("/", ".")


def hookwrap(fn, verbose):
    if getattr(fn, "wrapped", False):
        return fn

    def wrap(filename):
        frame = inspect.currentframe().f_back
        while "importlib" in frame.f_code.co_filename:
            frame = frame.f_back
        where = frame.f_code.co_filename
        print("load {} (where={})".format(normalize(filename), normalize(where)), file=sys.stderr)
        if verbose:
            traceback.print_stack(frame, limit=1, file=sys.stderr)
        return fn(filename)

    wrap.wrapped = True
    return wrap


def setup(verbose=False):
    for i in range(len(sys.path_hooks)):
        sys.path_hooks[i] = hookwrap(sys.path_hooks[i], verbose=verbose)
