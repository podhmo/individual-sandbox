from handofcats import as_command
from dictknife import loading
from dictknife import DictWalker
from dictknife import Accessor
from dictknife.jsonknife.accessor import access_by_json_pointer
from dictknife.jsonknife.resolver import get_resolver_from_filename
from dictknife.jsonknife import Bundler


@as_command
def main(*, src: str) -> None:
    precompile_ref_walker = DictWalker(["$precompile-ref"])
    accessor = Accessor()

    def onload(d, subresolver):
        for path, sd in precompile_ref_walker.walk(d):
            sdoc, query = subresolver.resolve(sd.pop("$precompile-ref"))
            sresolved = access_by_json_pointer(sdoc.doc, query)
            accessor.assign(d, path[:-1], sresolved)

    resolver = get_resolver_from_filename(src, onload=onload)
    d = Bundler(resolver).bundle()
    loading.dumpfile(d)
