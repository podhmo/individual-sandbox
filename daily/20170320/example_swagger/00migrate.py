import os.path
import yaml
import logging
from collections import OrderedDict
from collections import namedtuple
from dictknife.langhelpers import reify
from dictknife.walkers import LooseDictWalkingIterator, DataHandler
from dictknife import Accessor
from dictknife import Or


logger = logging.getLogger(__name__)
GlobalRef = namedtuple("GlobalRef", "path, q")


class Context(object):
    def __init__(self, path, loader, cache):
        self.path = os.path.normpath(os.path.abspath(path.replace("~", os.getenv("HOME"))))
        self.loader = loader
        self.cache = cache

    def resolve_path(self, relative_path):
        return os.path.normpath(os.path.join(os.path.dirname(self.path), relative_path))

    def load(self, path):
        if path not in self.cache:
            self.cache[path] = Context(path, self.loader, cache=self.cache)
        return self.cache[path]

    @reify
    def data(self):
        with open(self.path) as rf:
            return self.loader(rf)


class Migrator(object):
    def __init__(self):
        self.ctx_cache = OrderedDict()
        self.gref_cache = {}

    @reify
    def accessor(self):
        return Accessor()

    @reify
    def import_walker(self):
        return LooseDictWalkingIterator([Or(["x-bundler-concat", "x-bundler-compose"])], handler=DataHandler())

    @reify
    def ref_walker(self):
        return LooseDictWalkingIterator(["$ref"])

    def migrate(self, path, prefix=""):
        self.load(path)
        for gpath, ctx in self.ctx_cache.items():
            for path, d in self.ref_walker.iterate(ctx.data):
                try:
                    gref = self.lookup_global_ref(d["$ref"])
                except ValueError:
                    if not prefix:
                        raise
                    xs = d.split("/")
                    xs[-1] = xs[-1].replace(prefix, "")
                    try:
                        gref = self.lookup_global_ref(xs.join("/"))
                    except ValueError:
                        xs[-1][0] = xs[-1][0].lower()
                        gref = self.lookup_global_ref(xs.join("/"))
                if gref.path == ctx.path:
                    continue

                relative = os.path.relpath(gref.path, start=os.path.dirname(ctx.path))
                lref = "{}{}".format(relative, gref.q)
                # logger.debug("where=%s\n\told=%s\n\tnew=%s", ctx.path, d["$ref"], lref)
                d["$ref"] = lref
                print("gsed -i 's@{}@{}@' {}".format(d["$ref"], lref, ctx.path))

    def lookup_global_ref(self, q):
        if q in self.gref_cache:
            return self.gref_cache[q]
        # #/foo/bar -> ["foo", "bar"]
        path = q[2:].split("/")
        for gpath, ctx in self.ctx_cache.items():
            c = self.accessor.maybe_access_container(ctx.data, path)
            if c is not None:
                gref = self.gref_cache[q] = GlobalRef(path=ctx.path, q=q)
                return gref
        raise ValueError("not found {}".format(q))

    def load(self, path):
        ctx = Context(path, yaml.load, self.ctx_cache)
        self._load(ctx)
        return self.ctx_cache

    def _load(self, ctx):
        for q, relative_list in self.import_walker.iterate(ctx.data):
            for relative in relative_list:
                path = ctx.resolve_path(relative)
                if path not in ctx.cache:
                    self._load(ctx.load(path))


if __name__ == "__main__":
    import argparse

    logging.basicConfig(level=logging.DEBUG)
    parser = argparse.ArgumentParser()
    parser.add_argument("--prefix", default="")
    parser.add_argument("src")

    args = parser.parse_args()
    m = Migrator()
    m.migrate(args.src, prefix=args.prefix)
