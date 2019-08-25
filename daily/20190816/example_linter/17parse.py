import sys
import os.path
import logging
import subprocess
import yaml
from yaml.loader import (
    Reader,
    Scanner,
    Parser,
    Composer,
    SafeConstructor,
    Resolver,
    SafeLoader,
)
from dictknife.langhelpers import reify
import jsonschema

logger = logging.getLogger(__name__)


class NodeStore:
    def __init__(self):
        self.cache = {}

    def add_node(self, name, node, r):
        logger.debug("add_node %s", name)
        if r is None:
            return r
        self.cache[id(r)] = node
        return r

    def lookup_node(self, data):
        return self.cache[id(data)]


class WrappedConstructor(SafeConstructor):
    @reify
    def store(self):
        return NodeStore()

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
class YAMLLoader(Reader, Scanner, Parser, Composer, WrappedConstructor, Resolver):
    def __init__(self, stream):
        Reader.__init__(self, stream)
        Scanner.__init__(self)
        Parser.__init__(self)
        Composer.__init__(self)
        WrappedConstructor.__init__(self)
        Resolver.__init__(self)


class YAMLLoaderFactory:
    def __init__(self, loader_class):
        self.loader_class = loader_class
        self.store = NodeStore()

    def __call__(self, rf):
        loader = self.loader_class(rf)
        loader.store = self.store
        return loader


def main():
    filename = sys.argv[1]
    filename = os.path.abspath(filename)

    yaml_loader_factory = YAMLLoaderFactory(YAMLLoader)
    store = yaml_loader_factory.store

    with open(filename) as rf:
        data = yaml.load(rf, Loader=yaml_loader_factory)

    schema_filename = sys.argv[2]
    schema_filename = os.path.abspath(schema_filename)

    with open(schema_filename) as rf:
        schema = yaml.load(rf, Loader=SafeLoader)

    jsonschema.Draft7Validator.check_schema(schema)
    validator = jsonschema.Draft7Validator(schema=schema)
    errors = list(validator.iter_errors(data))
    print("?", len(errors))
    for e in errors:
        # print("path", e.path)
        # print("message", e.message)
        # print("instance", e.instance)
        # print(store.lookup_node(e.instance))
        # print("----------------------------------------")
        print(describe(store, e))
    else:
        print("----------------------------------------")
        subprocess.run(["cat", "-n", filename])


def describe(store: NodeStore, err: jsonschema.ValidationError) -> str:
    status = "ERROR"
    msg = f"{err.message} (validator={err.validator})"
    node = store.lookup_node(err.instance)

    start_mark, end_mark = node.start_mark, node.end_mark

    filename = os.path.relpath(start_mark.name, start=".")
    where = [os.path.relpath(filename)]
    where[0] = f"{where[0]}:{start_mark.line+1}"
    return f"status:{status}	cls:{err.__class__.__name__}	filename:{filename}	start:{start_mark.line+1}@{start_mark.column}	end:{end_mark.line+1}@{end_mark.column}	msg:{msg}	where:{where}"


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    main()
