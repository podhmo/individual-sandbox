import subprocess
import sys
import logging
import traceback
from yaml.loader import Reader, Scanner, Parser, Composer, Constructor, Resolver
from dictknife.langhelpers import reify

logger = logging.getLogger(__name__)


class Store:
    def __init__(self):
        self.node_cache = {}  # weak reference?
        self.path = []


class WrappedConstructor(Constructor):
    @reify
    def store(self):
        return Store()

    def wrap(self, path, name, node, r):
        # traceback.print_stack()
        d = 1
        default = 6
        n = len(traceback.extract_stack()) * d
        padding = " " * max((n - default), 0)
        logger.info("wrap %s%s", padding, name)
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

    def construct_scalar(self, node):
        self.store.path.append(node)
        r = super().construct_scalar(node)
        self.wrap(self.store.path, "construct_scalar", node, r)
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

    def construct_yaml_null(self, node):
        self.store.path.append(node)
        r = super().construct_yaml_null(node)
        self.wrap(self.store.path, "construct_yaml_null", node, r)
        self.store.path.pop()
        return r

    def construct_yaml_bool(self, node):
        self.store.path.append(node)
        r = super().construct_yaml_bool(node)
        self.wrap(self.store.path, "construct_yaml_bool", node, r)
        self.store.path.pop()
        return r

    def construct_yaml_int(self, node):
        self.store.path.append(node)
        r = super().construct_yaml_int(node)
        self.wrap(self.store.path, "construct_yaml_int", node, r)
        self.store.path.pop()
        return r

    def construct_yaml_float(self, node):
        self.store.path.append(node)
        r = super().construct_yaml_float(node)
        self.wrap(self.store.path, "construct_yaml_float", node, r)
        self.store.path.pop()
        return r

    def construct_yaml_binary(self, node):
        self.store.path.append(node)
        r = super().construct_yaml_binary(node)
        self.wrap(self.store.path, "construct_yaml_binary", node, r)
        self.store.path.pop()
        return r

    def construct_yaml_timestamp(self, node):
        self.store.path.append(node)
        r = super().construct_yaml_timestamp(node)
        self.wrap(self.store.path, "construct_yaml_timestamp", node, r)
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

    def construct_yaml_str(self, node):
        self.store.path.append(node)
        r = super().construct_yaml_str(node)
        self.wrap(self.store.path, "construct_yaml_str", node, r)
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

    def construct_undefined(self, node):
        self.store.path.append(node)
        r = super().construct_undefined(node)
        self.wrap(self.store.path, "construct_undefined", node, r)
        self.store.path.pop()
        return r

    def construct_python_str(self, node):
        self.store.path.append(node)
        r = super().construct_python_str(node)
        self.wrap(self.store.path, "construct_python_str", node, r)
        self.store.path.pop()
        return r

    def construct_python_unicode(self, node):
        self.store.path.append(node)
        r = super().construct_python_unicode(node)
        self.wrap(self.store.path, "construct_python_unicode", node, r)
        self.store.path.pop()
        return r

    def construct_python_bytes(self, node):
        self.store.path.append(node)
        r = super().construct_python_bytes(node)
        self.wrap(self.store.path, "construct_python_bytes", node, r)
        self.store.path.pop()
        return r

    def construct_python_long(self, node):
        self.store.path.append(node)
        r = super().construct_python_long(node)
        self.wrap(self.store.path, "construct_python_long", node, r)
        self.store.path.pop()
        return r

    def construct_python_complex(self, node):
        self.store.path.append(node)
        r = super().construct_python_complex(node)
        self.wrap(self.store.path, "construct_python_complex", node, r)
        self.store.path.pop()
        return r

    def construct_python_tuple(self, node):
        self.store.path.append(node)
        r = super().construct_python_tuple(node)
        self.wrap(self.store.path, "construct_python_tuple", node, r)
        self.store.path.pop()
        return r

    def construct_python_name(self, suffix, node):
        self.store.path.append(node)
        r = super().construct_python_name(suffix, node)
        self.wrap(self.store.path, "construct_python_name", node, r)
        self.store.path.pop()
        return r

    def construct_python_module(self, suffix, node):
        self.store.path.append(node)
        r = super().construct_python_module(suffix, node)
        self.wrap(self.store.path, "construct_python_module", node, r)
        self.store.path.pop()
        return r

    def construct_python_object(self, suffix, node):
        self.store.path.append(node)
        r = super().construct_python_object(suffix, node)
        self.wrap(self.store.path, "construct_python_object", node, r)
        self.store.path.pop()
        return r

    def construct_python_object_apply(self, suffix, node, newobj=False):
        self.store.path.append(node)
        r = super().construct_python_object_apply(suffix, node, newobj=newobj)
        self.wrap(self.store.path, "construct_python_object_apply", node, r)
        self.store.path.pop()
        return r

    def construct_python_object_new(self, suffix, node):
        self.store.path.append(node)
        r = super().construct_python_object_new(suffix, node)
        self.wrap(self.store.path, "construct_python_object_new", node, r)
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


def main():
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


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format="%(message)s")
    main()
