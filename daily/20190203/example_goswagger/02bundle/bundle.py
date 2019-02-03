from handofcats import as_command
from dictknife import loading
from dictknife.jsonknife import bundle, path_to_json_pointer, assign_by_json_pointer
from dictknife import DictWalker


@as_command
def main(*, src: str) -> None:
    precompile_ref_walker = DictWalker(["$precompile-ref"])

    def onload(d, resolver):
        for path, sd in precompile_ref_walker.walk(d):
            subresolver, query = resolver.resolve(sd.pop("$precompile-ref"))
            value = subresolver.access(query)
            jsref = path_to_json_pointer(path[:-1])
            assign_by_json_pointer(d, jsref, value)

    d = bundle(src, onload=onload)
    loading.dumpfile(d)
