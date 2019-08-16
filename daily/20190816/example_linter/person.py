import subprocess
import sys
from yaml.loader import Reader, Scanner, Parser, Composer, Constructor, Resolver
from dictknife.langhelpers import reify


class Store:
    def __init__(self):
        self.node_cache = {}  # weak reference?
        self.path = []


class WrappedConstructor(Constructor):
    @reify
    def store(self):
        return Store()

    def wrap(self, path, name, node, r):
        if r is None:
            return r
        self.store.node_cache[id(r)] = node
        return r

    def construct_document(self, node):
        self.store.path.append(node)
        r = super().construct_document(node)
        self.wrap(self.store.path, "construct_document", node, r)
        self.store.path.pop()
        return r

    def construct_object(self, node, deep=False):
        self.store.path.append(node)
        r = super().construct_object(node, deep=deep)
        self.wrap(self.store.path, "construct_object", node, r)
        self.store.path.pop()
        return r

    def construct_sequence(self, node, deep=False):
        self.store.path.append(node)
        r = super().construct_sequence(node, deep=deep)
        self.wrap(self.store.path, "construct_sequence", node, r)
        self.store.path.pop()
        return r

    def construct_mapping(self, node, deep=False):
        self.store.path.append(node)
        r = super().construct_mapping(node, deep=deep)
        self.wrap(self.store.path, "construct_mapping", node, r)
        self.store.path.pop()
        return r

    def construct_pairs(self, node, deep=False):
        self.store.path.append(node)
        r = super().construct_pairs(node, deep=deep)
        self.wrap(self.store.path, "construct_pairs", node, r)
        self.store.path.pop()
        return r

    def construct_yaml_omap(self, node):
        self.store.path.append(node)
        r = super().construct_yaml_omap(node)
        self.wrap(self.store.path, "construct_yaml_omap", node, r)
        self.store.path.pop()
        return r

    def construct_yaml_pairs(self, node):
        self.store.path.append(node)
        r = super().construct_yaml_pairs(node)
        self.wrap(self.store.path, "construct_yaml_pairs", node, r)
        self.store.path.pop()
        return r

    def construct_yaml_set(self, node):
        self.store.path.append(node)
        r = super().construct_yaml_set(node)
        self.wrap(self.store.path, "construct_yaml_set", node, r)
        self.store.path.pop()
        return r

    def construct_yaml_seq(self, node):
        self.store.path.append(node)
        r = super().construct_yaml_seq(node)
        self.wrap(self.store.path, "construct_yaml_seq", node, r)
        self.store.path.pop()
        return r

    def construct_yaml_map(self, node):
        self.store.path.append(node)
        r = super().construct_yaml_map(node)
        self.wrap(self.store.path, "construct_yaml_map", node, r)
        self.store.path.pop()
        return r

    def construct_yaml_object(self, node, cls):
        self.store.path.append(node)
        r = super().construct_yaml_object(node, cls)
        self.wrap(self.store.path, "construct_yaml_object", node, r)
        self.store.path.pop()
        return r


# copie from pyyaml
class Loader(Reader, Scanner, Parser, Composer, WrappedConstructor, Resolver):
    def __init__(self, stream):
        Reader.__init__(self, stream)
        Scanner.__init__(self)
        Parser.__init__(self)
        Composer.__init__(self)
        Constructor.__init__(self)
        Resolver.__init__(self)


# load
# - scan
# - parse
# - compose


if __name__ == "__main__":
    filename = sys.argv[1]
    with open(filename) as rf:
        loader = Loader(rf)
        try:
            assert loader.check_data()
            data = loader.get_data()
            node_cache = loader.store.node_cache

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
        finally:
            loader.dispose()
