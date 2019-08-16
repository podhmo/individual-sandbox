import subprocess
import sys
from yaml.loader import Reader, Scanner, Parser, Composer, Constructor, Resolver


mem = {}  # weak reference?
mem2 = {}
path = []


class WrappedConstructor(Constructor):
    def wrap(self, path, name, node, r):
        if r is None:
            return r
        # print("@", id(r), repr(r))
        # jsonpointer
        mem[id(r)] = node
        mem2[tuple(path[:])] = node
        return r

    def construct_document(self, node):
        path.append(node)
        r = super().construct_document(node)
        self.wrap(path, "construct_document", node, r)
        path.pop()
        return r

    def construct_object(self, node, deep=False):
        path.append(node)
        r = super().construct_object(node, deep=deep)
        self.wrap(path, "construct_object", node, r)
        path.pop()
        return r

    def construct_scalar(self, node):
        path.append(node)
        r = super().construct_scalar(node)
        self.wrap(path, "construct_scalar", node, r)
        path.pop()
        return r

    def construct_sequence(self, node, deep=False):
        path.append(node)
        r = super().construct_sequence(node, deep=deep)
        self.wrap(path, "construct_sequence", node, r)
        path.pop()
        return r

    def construct_mapping(self, node, deep=False):
        path.append(node)
        r = super().construct_mapping(node, deep=deep)
        self.wrap(path, "construct_mapping", node, r)
        path.pop()
        return r

    def construct_pairs(self, node, deep=False):
        path.append(node)
        r = super().construct_pairs(node, deep=deep)
        self.wrap(path, "construct_pairs", node, r)
        path.pop()
        return r

    def construct_yaml_null(self, node):
        path.append(node)
        r = super().construct_yaml_null(node)
        self.wrap(path, "construct_yaml_null", node, r)
        path.pop()
        return r

    def construct_yaml_bool(self, node):
        path.append(node)
        r = super().construct_yaml_bool(node)
        self.wrap(path, "construct_yaml_bool", node, r)
        path.pop()
        return r

    def construct_yaml_int(self, node):
        path.append(node)
        r = super().construct_yaml_int(node)
        self.wrap(path, "construct_yaml_int", node, r)
        path.pop()
        return r

    def construct_yaml_float(self, node):
        path.append(node)
        r = super().construct_yaml_float(node)
        self.wrap(path, "construct_yaml_float", node, r)
        path.pop()
        return r

    def construct_yaml_binary(self, node):
        path.append(node)
        r = super().construct_yaml_binary(node)
        self.wrap(path, "construct_yaml_binary", node, r)
        path.pop()
        return r

    def construct_yaml_timestamp(self, node):
        path.append(node)
        r = super().construct_yaml_timestamp(node)
        self.wrap(path, "construct_yaml_timestamp", node, r)
        path.pop()
        return r

    def construct_yaml_omap(self, node):
        path.append(node)
        r = super().construct_yaml_omap(node)
        self.wrap(path, "construct_yaml_omap", node, r)
        path.pop()
        return r

    def construct_yaml_pairs(self, node):
        path.append(node)
        r = super().construct_yaml_pairs(node)
        self.wrap(path, "construct_yaml_pairs", node, r)
        path.pop()
        return r

    def construct_yaml_set(self, node):
        path.append(node)
        r = super().construct_yaml_set(node)
        self.wrap(path, "construct_yaml_set", node, r)
        path.pop()
        return r

    def construct_yaml_str(self, node):
        path.append(node)
        r = super().construct_yaml_str(node)
        self.wrap(path, "construct_yaml_str", node, r)
        path.pop()
        return r

    def construct_yaml_seq(self, node):
        path.append(node)
        r = super().construct_yaml_seq(node)
        self.wrap(path, "construct_yaml_seq", node, r)
        path.pop()
        return r

    def construct_yaml_map(self, node):
        path.append(node)
        r = super().construct_yaml_map(node)
        self.wrap(path, "construct_yaml_map", node, r)
        path.pop()
        return r

    def construct_yaml_object(self, node, cls):
        path.append(node)
        r = super().construct_yaml_object(node, cls)
        self.wrap(path, "construct_yaml_object", node, r)
        path.pop()
        return r

    def construct_undefined(self, node):
        path.append(node)
        r = super().construct_undefined(node)
        self.wrap(path, "construct_undefined", node, r)
        path.pop()
        return r

    def construct_python_str(self, node):
        path.append(node)
        r = super().construct_python_str(node)
        self.wrap(path, "construct_python_str", node, r)
        path.pop()
        return r

    def construct_python_unicode(self, node):
        path.append(node)
        r = super().construct_python_unicode(node)
        self.wrap(path, "construct_python_unicode", node, r)
        path.pop()
        return r

    def construct_python_bytes(self, node):
        path.append(node)
        r = super().construct_python_bytes(node)
        self.wrap(path, "construct_python_bytes", node, r)
        path.pop()
        return r

    def construct_python_long(self, node):
        path.append(node)
        r = super().construct_python_long(node)
        self.wrap(path, "construct_python_long", node, r)
        path.pop()
        return r

    def construct_python_complex(self, node):
        path.append(node)
        r = super().construct_python_complex(node)
        self.wrap(path, "construct_python_complex", node, r)
        path.pop()
        return r

    def construct_python_tuple(self, node):
        path.append(node)
        r = super().construct_python_tuple(node)
        self.wrap(path, "construct_python_tuple", node, r)
        path.pop()
        return r

    def construct_python_name(self, suffix, node):
        path.append(node)
        r = super().construct_python_name(suffix, node)
        self.wrap(path, "construct_python_name", node, r)
        path.pop()
        return r

    def construct_python_module(self, suffix, node):
        path.append(node)
        r = super().construct_python_module(suffix, node)
        self.wrap(path, "construct_python_module", node, r)
        path.pop()
        return r

    def construct_python_object(self, suffix, node):
        path.append(node)
        r = super().construct_python_object(suffix, node)
        self.wrap(path, "construct_python_object", node, r)
        path.pop()
        return r

    def construct_python_object_apply(self, suffix, node, newobj=False):
        path.append(node)
        r = super().construct_python_object_apply(suffix, node, newobj=newobj)
        self.wrap(path, "construct_python_object_apply", node, r)
        path.pop()
        return r

    def construct_python_object_new(self, suffix, node):
        path.append(node)
        r = super().construct_python_object_new(suffix, node)
        self.wrap(path, "construct_python_object_new", node, r)
        path.pop()
        return r


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

            print("----------------------------------------")

            print(data)
            node = mem[id(data)]
            print(":", node.start_mark, getattr(node, "end_mark", None))

            print("----------------------------------------")

            k = next(reversed(list(data.keys())))
            print(k)
            node = mem[id(k)]
            print(":", node.start_mark, getattr(node, "end_mark", None))

            print("----------------------------------------")
            subprocess.run(["cat", "-n", sys.argv[1]])
        finally:
            loader.dispose()
