import sys
import os.path
import subprocess
import logging
from collections import ChainMap
from dictknife import DictWalker, Accessor
from dictknife import loading
from dictknife.jsonknife import get_resolver
from dictknife.jsonknife.accessor import StackedAccessor
from dictknife.langhelpers import make_dict
from yaml.error import MarkedYAMLError

logger = logging.getLogger(__name__)


class Expander:
    def __init__(self, resolver):
        self.resolver = resolver
        self.accessor = StackedAccessor(resolver)
        self.accessing = Accessor()
        self.ref_walking = DictWalker(["$ref"])

    def expand(self, doc=None, resolver=None, ctx=None):
        doc = doc or self.resolver.doc
        resolver = resolver or self.resolver

        if "$ref" in doc:
            original = self.accessor.access(doc["$ref"])
            new_doc = self.expand(
                original, resolver=self.accessor.resolver, ctx=ctx
            )
            self.accessor.pop_stack()
            return new_doc
        else:
            for path, sd in self.ref_walking.iterate(doc):
                new_sd = self.expand(sd, resolver=resolver, ctx=ctx)
                container = self.accessing.access(doc, path[:-1])
                if not hasattr(container, "parents"):
                    container = ChainMap(make_dict(), container)
                    container.update(new_sd)
                self.accessing.assign(doc, path[:-1], container)
            return doc


def main():
    filename = sys.argv[1]
    resolver = get_resolver(filename)
    try:
        expander = Expander(resolver)
        doc = expander.expand()
        loading.dumpfile(doc)  # with $ref
    except MarkedYAMLError as e:
        padding = ""
        mark = e.context_mark or e.problem_mark
        filename = mark.name
        for r in resolver.path_list(filename):
            padding += "  "
            print(padding, os.path.relpath(r.filename))

        padding += "  "
        print(padding, "problem", e.problem, "@", e.problem_mark)
        print(padding, "context", e.context, "@", e.context_mark)
        print("")

    print("----------------------------------------")
    subprocess.run(["cat", "-n", filename])


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG)
    main()
