from handofcats import as_command
from dictknife import loading
from dictknife.jsonknife import bundle
from dictknife import DictWalker


@as_command
def main(*, src: str) -> None:
    precompile_ref_walker = DictWalker(["$precompile-ref"])

    def onload(d, resolver):
        for path, sd in precompile_ref_walker.walk(d):
            subresolver, query = resolver.resolve(sd.pop("$precompile-ref"))
            sresolved = subresolver.access(query)
            subresolver.assign("/".join(path[:-1]), sresolved)

    d = bundle(src, onload=onload)
    loading.dumpfile(d)
