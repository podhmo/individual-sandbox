import sys
import inspect
import traceback


def hookwrap(fn, verbose):
    if getattr(fn, "wrapped", False):
        return fn

    def wrap(filename):
        print("load", filename, file=sys.stderr)
        frame = inspect.currentframe().f_back
        while "importlib" in frame.f_code.co_filename:
            frame = frame.f_back
        traceback.print_stack(frame, limit=1, file=sys.stderr)
        return fn(filename)

    wrap.wrapped = True
    return wrap


def setup(verbose=False):
    for i in range(len(sys.path_hooks)):
        sys.path_hooks[i] = hookwrap(sys.path_hooks[i], verbose=verbose)
