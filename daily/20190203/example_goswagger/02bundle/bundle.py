from handofcats import as_command
from dictknife import loading
from dictknife.jsonknife import bundle
from dictknife import DictWalker
from dictknife import Accessor
from dictknife.jsonknife.accessor import access_by_json_pointer


@as_command
def main(*, src: str) -> None:
    precompile_ref_walker = DictWalker(["$precompile-ref"])
    accessor = Accessor()

    def onload(d, subresolver):
        for path, sd in precompile_ref_walker.walk(d):
            sdoc, query = subresolver.resolve(sd.pop("$precompile-ref"))
            sresolved = access_by_json_pointer(sdoc.doc, query)
            accessor.assign(d, path[:-1], sresolved)

    d = bundle(src, onload=onload)
    loading.dumpfile(d)
