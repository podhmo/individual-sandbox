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
from dictknife.jsonknife.accessor import StackedAccessor, is_ref
from dictknife.langhelpers import make_dict, reify

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


class LintError(Exception):
    def __init__(
        self, inner: str, *, history: list, path: list = None, data: dict = None
    ):
        super().__init__(repr(inner))
        self.inner = inner
        self.history = history
        self.path = path
        self.data = data

    def __str__(self):
        return f"{self.__class__.__name__}: {self.inner}"


class ParseError(LintError):
    # usually, inner is MarkedYAMLError
    pass


class ResolutionError(LintError):
    # usually, inner is KeyError or FileNotFoundError
    pass


class DataScanner:  # todo: rename
    def __init__(self, resolver):
        self.resolver = resolver
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
                self.errors.append(ParseError(e, history=[resolver.filename]))
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
                except FileNotFoundError as e:
                    self.errors.append(
                        ResolutionError(
                            e,
                            path=path[:],
                            data=sd,
                            history=[r.filename for r in self.accessor.stack[:-1]],
                        )
                    )
                except KeyError as e:
                    self.errors.append(
                        ResolutionError(
                            e,
                            path=path[:],
                            data=sd,
                            history=[r.filename for r in self.accessor.stack],
                        )
                    )
                except MarkedYAMLError as e:
                    if e.problem_mark is not None:
                        self.errors.append(
                            ParseError(
                                e,
                                path=path[:],
                                data=sd,
                                history=[r.filename for r in self.accessor.stack],
                            )
                        )
            return doc, resolver


class _Adapter:
    def __init__(self, yamlloader_factory):
        self.yamlloader_factory = yamlloader_factory

    def loadfile(self, filename, *, format=None):
        with open(filename) as rf:
            return yaml.load(rf, Loader=self.yamlloader_factory)


class Detector:
    def __init__(self, filename: str, *, store: NodeStore):
        self.filename = filename  # root file
        self.store = store

    def has_error_point(self, err: LintError):
        return getattr(err, "problem_mark", None) is not None

    def detect_status(self, filename):
        if self.filename == filename:
            return "ERROR"
        else:
            return "WARNING"

    def detect_scanning_start_point(self, err: LintError) -> (Mark, Mark):
        if err.data is None:
            return self.detect_error_point(err)
        map_node = self.store.lookup_node(err.data)
        knode, vnode = self.lookup_kvpair(map_node, err.path[-1])
        return knode.start_mark, vnode.end_mark

    def detect_error_point(self, err: LintError) -> Mark:
        mark = getattr(err.inner, "context_mark")
        import copy

        start_mark = copy.deepcopy(mark)
        start_mark.column = 0
        end_mark = copy.deepcopy(mark)
        end_mark.column = -1
        return (start_mark, end_mark)

    def lookup_kvpair(self, node, k):  # todo: rename
        for knode, vnode in node.value:
            if knode.value == k:
                return knode, vnode


class Describer:
    def __init__(self, filename: str, *, store: NodeStore):
        self.filename = filename
        self.store = store
        self.detector = Detector(filename, store=store)

    def describe(self, err: LintError) -> str:
        if isinstance(err, ParseError):
            return self.describe_parse_error(err)
        elif isinstance(err, ResolutionError):
            return self.describe_resolution_error(err)
        else:
            raise err

    def describe_parse_error(self, err: ParseError) -> str:
        status = self.detector.detect_status(err.history[-1])
        if hasattr(err.inner, "problem"):
            msg = f"{err.inner.problem} ({err.inner.context})"
        else:
            msg = repr(err.inner)

        start_mark, end_mark = self.detector.detect_scanning_start_point(err)
        filename = os.path.relpath(start_mark.name, start=".")

        where = [os.path.relpath(name) for name in err.history]
        where[0] = f"{where[0]}:{start_mark.line+1}"
        if self.detector.has_error_point(err):
            where[-1] = f"{where[-1]}:{err.inner.problem_mark.line+1}"

        return f"status:{status}	cls:{err.__class__.__name__}	filename:{filename}	start:{start_mark.line+1}@{start_mark.column}	end:{end_mark.line+1}@{end_mark.column}	msg:{msg}	where:{where}"

    def describe_resolution_error(self, err: ResolutionError) -> str:
        start_mark, end_mark = self.detector.detect_scanning_start_point(err)
        filename = os.path.relpath(start_mark.name, start=".")
        status = self.detector.detect_status(err.history[-1])
        msg = repr(err.inner)

        where = [os.path.relpath(name) for name in err.history]
        where[0] = f"{where[0]}:{start_mark.line+1}"
        if self.detector.has_error_point(err):
            where[-1] = f"{where[-1]}:{err.inner.problem_mark.line+1}"
        return f"status:{status}	cls:{err.__class__.__name__}	filename:{filename}	start:{start_mark.line+1}@{start_mark.column}	end:{end_mark.line+1}@{end_mark.column}	msg:{msg}	where:{where}"


def main():
    filename = sys.argv[1]
    filename = os.path.abspath(filename)
    yaml_loader_factory = YAMLLoaderFactory(YAMLLoader)

    resolver = get_resolver(filename, loader=_Adapter(yaml_loader_factory))
    scanner = DataScanner(resolver)
    doc = scanner.scan()
    print("----------------------------------------")
    subprocess.run(["cat", "-n", filename])
    # from dictknife import loading
    # loading.dumpfile(doc)

    if scanner.errors:
        describer = Describer(filename, store=yaml_loader_factory.store)
        print("?", len(scanner.errors))
        for err in scanner.errors:
            print(describer.describe(err))


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    main()
