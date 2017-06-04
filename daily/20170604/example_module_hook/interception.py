import sys
import inspect
from importlib.machinery import SourceFileLoader, SourcelessFileLoader


def get_caller(lv=1):
    frame = inspect.currentframe()
    for _ in range(lv):
        frame = frame.f_back
    while "importlib" in frame.f_code.co_filename:
        frame = frame.f_back
    return frame


def show_caller(filename, lv=2):
    frame = get_caller(lv)
    where = frame.f_code.co_filename
    print(
        "@@ load {} (where={})".format(normalize(filename), normalize(where)), file=sys.stderr
    )


class ShowCallerSourcelessFileLoader(SourcelessFileLoader):
    def exec_module(self, module):
        show_caller(module.__file__, lv=3)
        return super().exec_module(module)


class ShowCallerSourceFileLoader(SourceFileLoader):
    def exec_module(self, module):
        show_caller(module.__file__, lv=3)
        return super().exec_module(module)


def normalize(name):
    for path in sys.path:
        name = name.replace(path, "")
    return name


def hookwrap(fn, verbose):
    if getattr(fn, "wrapped", False):
        return fn

    def wrap(filename):
        finder = fn(filename)
        finder._loaders = get_new_loaders(finder._loaders)
        return finder

    def get_new_loaders(loaders):
        r = []
        for pair in loaders:
            if pair[1] == SourceFileLoader:
                r.append((pair[0], ShowCallerSourceFileLoader))
            elif pair[1] == SourcelessFileLoader:
                r.append((pair[0], ShowCallerSourcelessFileLoader))
            else:
                r.append(pair)
        return r

    wrap.wrapped = True
    return wrap


def setup(verbose=False):
    for i in range(len(sys.path_hooks)):
        sys.path_hooks[i] = hookwrap(sys.path_hooks[i], verbose=verbose)
