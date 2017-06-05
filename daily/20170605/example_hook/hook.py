import sys
DEFAULT_MODULES = set(sys.modules.keys())  # NOQA
import inspect


def _get_caller(frame=None, lv=2):
    frame = frame or inspect.currentframe()
    for _ in range(lv):
        frame = frame.f_back
        if frame is None:
            return None
    while "importlib" in frame.f_code.co_filename:
        frame = frame.f_back
    return frame


class FnedLoaderFactory:
    def __init__(self, fn):
        self.fn = fn
        self.cls_map = {}

    def create_ext_loader_class(self, cls, fn):
        if cls in self.cls_map:
            return self.cls_map[cls]

        class ExtLoader(cls):
            def exec_module(self, module):
                frame = _get_caller(lv=2)
                where = frame.f_code.co_filename
                filename = module.__file__
                fn(where, filename)
                return super().exec_module(module)

        self.cls_map[cls] = ExtLoader
        return ExtLoader

    def new_loaders(self, loaders):
        r = []
        for pair in loaders:
            r.append((pair[0], self.create_ext_loader_class(pair[1], self.fn)))
        return r

    def hookwrap(self, fn):
        if getattr(fn, "wrapped", False):
            return fn

        def wrap(filename):
            finder = fn(filename)
            finder._loaders = self.new_loaders(finder._loaders)
            return finder

        wrap.wrapped = True
        return wrap


def setup(fn):
    factory = FnedLoaderFactory(fn)

    for k in list(sys.modules.keys()):
        if k not in DEFAULT_MODULES:
            del sys.modules[k]

    for finder in sys.path_importer_cache.values():
        if finder and hasattr(finder, "_loaders"):
            finder._loaders = factory.new_loaders(finder._loaders)

    for i in range(len(sys.path_hooks)):
        sys.path_hooks[i] = factory.hookwrap(sys.path_hooks[i])
