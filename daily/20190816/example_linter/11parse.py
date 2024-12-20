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

    def lookup_node(self, data):
        return self.node_cache[id(data)]


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


class ReferenceError(Exception):
    def __init__(self, inner: Exception, *, path: list, data: dict) -> None:
        self.inner = inner
        self.path = path
        self.data = data
        super().__init__(repr(inner))

    def __str__(self):
        return f"{self.__class__.__name__}: {self.inner}"

    def lookup_node(self, store):  # todo: rename
        return store.lookup_node(self.data)

    def lookup_kvpair(self, node):  # todo: rename
        for knode, vnode in node.value:
            if knode.value == self.path[-1]:
                return knode, vnode


class Expander:
    def __init__(self, resolver):
        self.resolver = resolver
        self.accessor = StackedAccessor(resolver)
        self.accessing = Accessor()
        self.ref_walking = DictWalker(["$ref"])
        self.errors = []

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
                try:
                    new_sd = self.expand(sd, resolver=resolver, ctx=ctx)
                    container = self.accessing.access(doc, path[:-1])
                    if not hasattr(container, "parents"):
                        container = ChainMap(make_dict(), container)
                        container.update(new_sd)
                    self.accessing.assign(doc, path[:-1], container)
                except Exception as e:
                    self.errors.append(ReferenceError(e, path=path[:], data=sd))
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
    expander = Expander(resolver)

    try:
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

    store = yaml_loader_factory.store
    if expander.errors:
        print("?", len(expander.errors))
        for err in expander.errors:  # type: ReferenceError
            map_node = err.lookup_node(store)
            knode, vnode = err.lookup_kvpair(map_node)
            print(
                "!!",
                knode.start_mark,
                knode.end_mark,
                "x",
                vnode.start_mark,
                vnode.end_mark,
            )

    print("----------------------------------------")
    subprocess.run(["cat", "-n", filename])


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)
    main()
