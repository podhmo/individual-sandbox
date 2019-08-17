import sys
import os.path
import subprocess
import logging
import yaml
from yaml.error import MarkedYAMLError, Mark
from yaml.loader import Reader, Scanner, Parser, Composer, SafeConstructor, Resolver
from collections import ChainMap
from dictknife import DictWalker, Accessor
from dictknife.jsonknife import get_resolver
from dictknife.jsonknife.accessor import StackedAccessor, path_to_json_pointer, is_ref
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


class LintError(Exception):
    def __init__(
        self, inner: str, *, store=Store, path: list = None, data: dict = None
    ):
        super().__init__(repr(inner))
        self.inner = inner
        self.store = store
        self.path = path
        self.data = data

    def __str__(self):
        return f"{self.__class__.__name__}: {self.inner}"

    def describe(self) -> str:
        if self.data is not None:
            return self.describe_by_node()
        else:
            return self.describe_by_inner()  # original

    def describe_by_inner(self) -> str:
        status = "Error"  # xxx
        if hasattr(self.inner, "problem"):
            msg = f"{self.inner.problem} ({self.inner.context})"
        else:
            msg = repr(self.inner)
        mark = self.inner.problem_mark
        filename = os.path.relpath(mark.name, start=".")
        return f"status:{status}	cls:{self.__class__.__name__}	filename:{filename}	start:{mark.line}@0	end:{mark.line}@-1	msg:{msg}	where:{tuple(os.path.relpath(name) for name in reversed(self.inner.stack))}"

    def describe_by_node(self) -> str:
        map_node = self.node
        knode, vnode = self.lookup_kvpair(map_node)

        status = "Error"  # xxx
        filename = os.path.relpath(knode.start_mark.name, start=".")
        if hasattr(self.inner, "problem"):
            msg = f"{self.inner.problem} ({self.inner.context})"
        else:
            msg = repr(self.inner)
        return f"status:{status}	cls:{self.__class__.__name__}	filename:{filename}	start:{knode.start_mark.line+1}@{vnode.start_mark.column}	end:{vnode.end_mark.line+1}@{vnode.end_mark.column}	msg:{msg}	where:{tuple(os.path.relpath(name) for name in reversed(self.inner.stack))}"

    @property
    def node(self):  # todo: rename
        return self.store.lookup_node(self.data)

    def lookup_kvpair(self, node):  # todo: rename
        for knode, vnode in node.value:
            if knode.value == self.path[-1]:
                return knode, vnode


class ParseError(LintError):
    pass


class ReferenceError(LintError):
    pass


class Scaner:
    def __init__(self, resolver, *, store: Store):
        self.resolver = resolver
        self.store = store

        self.accessor = StackedAccessor(resolver)
        self.accessing = Accessor()
        self.ref_walking = DictWalker([is_ref])
        self.errors = []

    def scan(self, doc=None, resolver=None):
        if not doc and doc is not None:
            return doc
        resolver = resolver or self.resolver
        try:
            doc = doc or resolver.doc
        except MarkedYAMLError as e:
            if e.problem_mark is not None:
                self.errors.append(ParseError(e, store=self.store))
            if doc is None:
                doc = {}
        doc, _ = self._scan(doc, resolver=resolver, seen={})
        return doc

    def _scan(self, doc, *, resolver, seen: dict):
        if "$ref" in doc:
            original = self.accessor.access(doc["$ref"])
            new_doc, _ = self._scan(
                original, resolver=self.accessor.resolver, seen=seen
            )
            return new_doc, self.accessor.pop_stack()
        else:
            for path, sd in self.ref_walking.iterate(doc):
                try:
                    uid = id(sd)
                    if uid in seen:
                        continue

                    seen[uid] = sd
                    new_sd, sresolver = self._scan(sd, resolver=resolver, seen=seen)
                    if resolver.filename != sresolver.filename:
                        container = self.accessing.access(doc, path[:-1])
                        if not hasattr(container, "parents"):
                            container = ChainMap(make_dict(), container)
                            container.update(new_sd)
                        self.accessing.assign(doc, path[:-1], container)
                except (KeyError, FileNotFoundError) as e:
                    self.errors.append(
                        ReferenceError(e, store=self.store, path=path[:], data=sd)
                    )
                except MarkedYAMLError as e:
                    if e.problem_mark is not None:
                        self.errors.append(
                            ParseError(e, store=self.store, path=path[:], data=sd)
                        )
            return doc, resolver


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
    scaner = Scaner(resolver, store=yaml_loader_factory.store)
    doc = scaner.scan()
    print("----------------------------------------")
    subprocess.run(["cat", "-n", filename])
    # from dictknife import loading
    # loading.dumpfile(doc)

    if scaner.errors:
        print("?", len(scaner.errors))
        for err in scaner.errors:  # type: ReferenceError
            print(err.describe())


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    main()
