import subprocess
import sys
import logging
import yaml
from yaml.loader import Reader, Scanner, Parser, Composer, SafeConstructor, Resolver
from dictknife.langhelpers import reify

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


def desc(data, node_cache):
    print("----------------------------------------")

    print(data)
    node = node_cache[id(data)]
    print(":", node.start_mark, getattr(node, "end_mark", None))

    print("----------------------------------------")

    k = data["parents"][0]
    print(k)
    node = node_cache[id(k)]
    print(":", node.start_mark, getattr(node, "end_mark", None))

    print("----------------------------------------")
    subprocess.run(["cat", "-n", sys.argv[1]])


def main():
    filename = sys.argv[1]
    loader_factory = LoaderFactory(Loader)

    with open(filename) as rf:
        data = yaml.load(rf, Loader=loader_factory)

    node_cache = loader_factory.store.node_cache
    desc(data, node_cache)


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format="%(message)s")
    main()
