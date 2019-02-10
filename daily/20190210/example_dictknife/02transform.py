import os.path
import contextlib
from collections import ChainMap
import logging
from dictknife.jsonknife import get_resolver
from dictknife.jsonknife.bundler import Scanner, CachedItemAccessor
from dictknife.langhelpers import make_dict, pairrsplit
from dictknife.diff import diff
from handofcats import as_command

logger = logging.getLogger(__name__)


class Migration:
    def __init__(self, resolver, *, make_dict=make_dict):
        self.resolver = resolver
        self.accessor = CachedItemAccessor(resolver)
        self.make_dict = make_dict
        self.item_map = make_dict()

    def _prepare(self, doc=None):
        doc = doc or self.resolver.doc
        scanner = Scanner(self.accessor, self.item_map, strict=True)
        scanner.scan(self.resolver.doc)

    @contextlib.contextmanager
    def migrate_dryrun_and_diff(self, doc=None, *, where=None):
        where = where or os.getcwd()
        logger.debug("prepare (where=%s)", where)
        self._prepare(doc)

        yield self

        resolvers = set(item.resolver for item in self.item_map.values())
        logger.info("migrate dry run and diff")
        for r in resolvers:
            filename = os.path.relpath(r.name, start=where)
            before = self.before_data(r.doc)
            after = self.after_data(r.doc)
            for line in diff(
                before,
                after,
                fromfile=f"before:{filename}",
                tofile=f" after:{filename}",
            ):
                print(line)

    def migrate(self, doc=None, *, dry_run=False, where=None):
        if dry_run:
            return self.migrate_dryrun_and_diff(doc=doc, where=where)

        @contextlib.contextmanager
        def _migrate():
            raise NotImplementedError("hmm")

        return _migrate()

    def update(self, resolver, ref, v):
        parent_ref, k = pairrsplit(ref, "/")
        d = resolver.access_by_json_pointer(parent_ref)
        if not hasattr(d, "parents"):  # chainmap?
            d = ChainMap(self.make_dict(), d)
            resolver.assign_by_json_pointer(parent_ref, d)
        d[k] = v

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
    logging.basicConfig(level=logging.DEBUG)

    resolver = get_resolver(src)
    with Migration(resolver).migrate(dry_run=True) as m:
        item_map = m.item_map
        for k, item in list(item_map.items()):
            if k == "definitions/person":
                ref = "#/definitions/person/properties/value"
                m.update(item.resolver, ref, {"type": "integer"})
            if k == "definitions/name":
                ref = "#/definitions/name/description"
                m.update(item.resolver, ref, "name of something")
