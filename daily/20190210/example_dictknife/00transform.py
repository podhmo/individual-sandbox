import os.path
from collections import ChainMap
from dictknife.jsonknife import get_resolver
from dictknife.jsonknife.bundler import Scanner, CachedItemAccessor
from dictknife.langhelpers import make_dict
from dictknife.diff import diff
from handofcats import as_command


def before_data(d):
    if hasattr(d, "parents"):
        return {k: before_data(v) for k, v in d.parents.items()}
    elif hasattr(d, "keys"):
        return {k: before_data(v) for k, v in d.items()}
    elif isinstance(d, (list, tuple)):
        return [before_data(x) for x in d]
    else:
        return d


def after_data(d):
    if hasattr(d, "keys"):
        return {k: after_data(v) for k, v in d.items()}
    elif isinstance(d, (list, tuple)):
        return [after_data(x) for x in d]
    else:
        return d


@as_command
def run(*, src: str) -> None:
    resolver = get_resolver(src)
    scanner = Scanner(accessor, item_map, strict=True)
    scanner.scan(resolver.doc)

    resolvers = set()
    for k, item in list(item_map.items()):
        if k == "definitions/person":
            item.data["properties"] = ChainMap(make_dict(), item.data["properties"])
            item.data["properties"]["value"] = {"type": "integer"}
        if k == "definitions/name":
            item.data = ChainMap(make_dict(), item.data)
            item.resolver.assign_by_json_pointer(item.localref, item.data)
            item.data["description"] = "name of something"
        resolvers.add(item.resolver)

    for r in resolvers:
        print(os.path.relpath(r.name, start=os.getcwd()))
        for line in diff(before_data(r.doc), after_data(r.doc)):
            print(line)
