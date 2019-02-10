import os.path
import contextlib
from collections import ChainMap
from dictknife.jsonknife import get_resolver
from dictknife.jsonknife.bundler import Scanner, CachedItemAccessor
from dictknife.langhelpers import make_dict
from dictknife.diff import diff
from handofcats import as_command


class Migration:
    def __init__(self, resolver):
        self.resolver = resolver
        self.accessor = CachedItemAccessor(resolver)
        self.item_map = make_dict()

    def _prepare(self, doc=None):
        doc = doc or self.resolver.doc
        scanner = Scanner(self.accessor, self.item_map, strict=True)
        scanner.scan(self.resolver.doc)

    @contextlib.contextmanager
    def migrate_dryrun_and_diff(self, doc=None):
        self._prepare(doc)
        yield self
        resolvers = set(item.resolver for item in self.item_map.values())
        for r in resolvers:
            print(os.path.relpath(r.name, start=os.getcwd()))
            for line in diff(self.before_data(r.doc), self.after_data(r.doc)):
                print(line)

    def before_data(self, d):
        if hasattr(d, "parents"):
            return {k: self.before_data(v) for k, v in d.parents.items()}
        elif hasattr(d, "keys"):
            return {k: self.before_data(v) for k, v in d.items()}
        elif isinstance(d, (list, tuple)):
            return [self.before_data(x) for x in d]
        else:
            return d

    def after_data(self, d):
        if hasattr(d, "keys"):
            return {k: self.after_data(v) for k, v in d.items()}
        elif isinstance(d, (list, tuple)):
            return [self.after_data(x) for x in d]
        else:
            return d


@as_command
def run(*, src: str) -> None:
    resolver = get_resolver(src)

    with Migration(resolver).migrate_dryrun_and_diff() as m:
        item_map = m.item_map
        for k, item in list(item_map.items()):
            if k == "definitions/person":
                item.data["properties"] = ChainMap(make_dict(), item.data["properties"])
                item.data["properties"]["value"] = {"type": "integer"}
            if k == "definitions/name":
                item.data = ChainMap(make_dict(), item.data)
                item.resolver.assign_by_json_pointer(item.localref, item.data)
                item.data["description"] = "name of something"
