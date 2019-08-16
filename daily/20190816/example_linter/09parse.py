import sys
import os.path
import subprocess
import logging
import yaml
from yaml.error import MarkedYAMLError
from yaml.loader import Reader, Scanner, Parser, Composer, SafeConstructor, Resolver
from collections import ChainMap
from dictknife import DictWalker, Accessor
from dictknife import loading
from dictknife.jsonknife import get_resolver
from dictknife.jsonknife.accessor import StackedAccessor
from dictknife.langhelpers import make_dict, reify

logger = logging.getLogger(__name__)


class Store:
    def __init__(self):
        self.node_cache = {}  # weak reference?
        # need path ?

    def add_node(self, name, node, r):
        logger.debug("add_node %s", name)
        if r is None:
            return r
        self.node_cache[id(r)] = node
        return r


class WrappedConstructor(SafeConstructor):
    @reify
    def store(self):
        return Store()

    def construct_object(self, node, deep=False):
        r = super().construct_object(node, deep=deep)
        self.store.add_node("construct_object", node, r)
        return r

    def construct_sequence(self, node, deep=False):
        r = super().construct_sequence(node, deep=deep)
        self.store.add_node("construct_sequence", node, r)
        return r

    def construct_mapping(self, node, deep=False):
        r = super().construct_mapping(node, deep=deep)
        self.store.add_node("construct_mapping", node, r)
        return r


# copie from pyyaml
class Loader(Reader, Scanner, Parser, Composer, WrappedConstructor, Resolver):
    def __init__(self, stream):
        Reader.__init__(self, stream)
        Scanner.__init__(self)
        Parser.__init__(self)
        Composer.__init__(self)
        WrappedConstructor.__init__(self)
        Resolver.__init__(self)


class LoaderFactory:
    def __init__(self, loader_class):
        self.loader_class = loader_class
        self.store = Store()

    def __call__(self, rf):
        loader = self.loader_class(rf)
        loader.store = self.store
        return loader


class Expander:
    def __init__(self, resolver):
        self.resolver = resolver
        self.accessor = StackedAccessor(resolver)
        self.accessing = Accessor()
        self.ref_walking = DictWalker(["$ref"])

    def expand(self, doc=None, resolver=None, ctx=None):
        doc = doc or self.resolver.doc
        resolver = resolver or self.resolver

        if "$ref" in doc:
            original = self.accessor.access(doc["$ref"])
            new_doc = self.expand(original, resolver=self.accessor.resolver, ctx=ctx)
            self.accessor.pop_stack()
            return new_doc
        else:
            for path, sd in self.ref_walking.iterate(doc):
                new_sd = self.expand(sd, resolver=resolver, ctx=ctx)
                container = self.accessing.access(doc, path[:-1])
                if not hasattr(container, "parents"):
                    container = ChainMap(make_dict(), container)
                    container.update(new_sd)
                self.accessing.assign(doc, path[:-1], container)
            return doc


class _Adapter:
    def __init__(self, yamlloader_factory):
        self.yamlloader_factory = yamlloader_factory

    def loadfile(self, filename, *, format=None):
        with open(filename) as rf:
            return yaml.load(rf, Loader=self.yamlloader_factory)


def main():
    filename = sys.argv[1]
    yaml_loader_factory = LoaderFactory(Loader)

    resolver = get_resolver(filename, loader=_Adapter(yaml_loader_factory))
    doc = resolver.doc

    try:
        expander = Expander(resolver)
        doc = expander.expand()
        loading.dumpfile(doc)  # with $ref
    except MarkedYAMLError as e:
        padding = ""
        mark = e.context_mark or e.problem_mark
        filename = mark.name
        for r in resolver.path_list(filename):
            padding += "  "
            print(padding, os.path.relpath(r.filename))

        padding += "  "
        print(padding, "problem", e.problem, "@", e.problem_mark)
        print(padding, "context", e.context, "@", e.context_mark)
        print("")

    print("----------------------------------------")
    subprocess.run(["cat", "-n", filename])

    node_cache = yaml_loader_factory.store.node_cache

    sd = doc["components"]["schemas"]["a"]
    if hasattr(sd, "maps"):
        sd = sd.maps[-1]  # find original (in ChainMap)
    node = node_cache[id(sd)]

    print("@", node.start_mark)
    print("@", node.end_mark)


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    main()
