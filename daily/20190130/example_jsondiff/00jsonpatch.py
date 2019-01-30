from handofcats import as_command
from dictknife import loading
import jsonpatch


@as_command
def main(*, src: str, dst: str) -> None:
    src = loading.loadfile(src)
    dst = loading.loadfile(dst)
    patches = jsonpatch.make_patch(src, dst).patch
    loading.dumpfile(patches, format="json")
