from dictknife import loading
from dictknife.jsonknife import get_resolver
from dictknife.jsonknife import Bundler
from dictknife import DictWalker
import logging
logging.basicConfig(level=logging.DEBUG)


def build_subset(resolver, ref):
    subset = {}
    ob = resolver.access_by_json_pointer(ref)
    resolver.assign_by_json_pointer(ref, ob, doc=subset)
    for path, sd in DictWalker(["$ref"]).walk(ob):
        # xxx:
        if sd["$ref"].startswith("#/"):
            resolver.assign(path[:-1], sd, doc=subset)
    return subset


filename = "./src/person.json"
r = get_resolver(filename)
subset = build_subset(r, "/definitions/person")
b = Bundler(r)
d = b.bundle(subset)
loading.dumpfile(d)
