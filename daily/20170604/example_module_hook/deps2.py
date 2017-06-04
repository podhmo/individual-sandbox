import sys
import inspect
import contextlib
from collections import defaultdict
from prestring import Module


def get_caller(frame=None, lv=2):
    frame = frame or inspect.currentframe()
    for _ in range(lv):
        frame = frame.f_back
        if frame is None:
            return None
    while "importlib" in frame.f_code.co_filename:
        frame = frame.f_back
    return frame


class HookedLoaderFactory:
    def __init__(self, hook):
        self.hook = hook
        self.cls_map = {}

    def create_ext_loader_class(self, cls, hook):
        if cls in self.cls_map:
            return self.cls_map[cls]

        class ExtLoader(cls):
            def exec_module(self, module):
                frame = get_caller(lv=2)
                where = frame.f_code.co_filename
                filename = module.__file__
                hook(where, filename)

                # xxx:
                i = 0
                while True:
                    i += 1
                    filename = where
                    frame = get_caller(frame, lv=2)
                    if frame is None:
                        break
                    where = frame.f_code.co_filename
                    if where == __file__:
                        break
                    hook(where, filename)
                return super().exec_module(module)

        self.cls_map[cls] = ExtLoader
        return ExtLoader

    def new_loaders(self, loaders):
        r = []
        for pair in loaders:
            r.append((pair[0], self.create_ext_loader_class(pair[1], self.hook)))
        return r

    def hookwrap(self, fn, verbose):
        if getattr(fn, "wrapped", False):
            return fn

        def wrap(filename):
            finder = fn(filename)
            finder._loaders = self.new_loaders(finder._loaders)
            return finder

        wrap.wrapped = True
        return wrap


class DotModule(Module):
    @contextlib.contextmanager
    def block(self, value=""):
        self.stmt("{}{{".format(value))
        with self.scope():
            yield
        self.stmt("}")


class GensymMap:
    def __init__(self, prefix="g"):
        self.prefix = prefix
        self.d = {}
        self.i = 0

    def __contains__(self, name):
        return name in self.d

    def __getitem__(self, name):
        if name in self.d:
            return self.d[name]
        self.d[name] = v = "{}{}".format(self.prefix, self.i)
        self.i += 1
        return v


class Dag:
    def __init__(self):
        self.nodes = defaultdict(set)
        self.gensym_map = GensymMap("g")

    def _add(self, src, dst):
        if src == dst:
            return
        self.nodes[src].add(dst)

    def add(self, src, dst):
        src = normalize(src)
        dst = normalize(dst)
        return self._add(src, dst)

    def to_dot(self):
        m = DotModule()
        with m.block("digraph g "):
            for src, dsts in self.nodes.items():
                if src not in self.gensym_map:
                    m.stmt('{} [label="{}"]'.format(self.gensym_map[src], src))
                for dst in dsts:
                    if dst not in self.gensym_map:
                        m.stmt('{} [label="{}"]'.format(self.gensym_map[dst], dst))
            for src, dsts in self.nodes.items():
                for dst in dsts:
                    m.stmt("{} -> {}", self.gensym_map[src], self.gensym_map[dst])
        return str(m)


class GroupedDag(Dag):
    def _add(self, src, dst):
        L = list(reversed(src.split(".")))
        for i in range(len(L)):
            newsrc = ".".join(reversed(L[i:]))
            super()._add(newsrc, dst)


def normalize(name):
    for path in sys.path:
        name = name.replace(path, "")
    return name.lstrip("/").rsplit("/__init__.py", 1)[0].rsplit(".py", 1)[0].replace("/", ".")


def display(where, filename):
    print("@@load {} (where={})".format(normalize(filename), normalize(where)))


def setup(verbose=False, hook=display):
    factory = HookedLoaderFactory(hook)
    for i in range(len(sys.path_hooks)):
        sys.path_hooks[i] = factory.hookwrap(sys.path_hooks[i], verbose=verbose)
