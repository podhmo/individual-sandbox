import sys
import inspect
import traceback
import contextlib
from collections import defaultdict
from prestring import Module


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


def hookwrap(fn, verbose, hook):
    if getattr(fn, "wrapped", False):
        return fn

    def wrap(filename):
        frame = inspect.currentframe().f_back
        while "importlib" in frame.f_code.co_filename:
            frame = frame.f_back
        where = frame.f_code.co_filename
        hook(where, filename)
        if verbose:
            traceback.print_stack(frame, limit=1, file=sys.stderr)
        return fn(filename)

    wrap.wrapped = True
    return wrap


def display(where, filename):
    print("load {} (where={})".format(normalize(filename), normalize(where)), file=sys.stderr)


def setup(verbose=False, hook=display):
    for i in range(len(sys.path_hooks)):
        sys.path_hooks[i] = hookwrap(sys.path_hooks[i], verbose=verbose, hook=hook)
