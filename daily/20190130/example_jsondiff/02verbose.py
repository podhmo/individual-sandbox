from handofcats import as_command
from dictknife import loading
from magicalimport import import_from_physical_path
unpatch = import_from_physical_path("../example_jsonpatch/00unpatch.py")


@as_command
def main(*, src: str, dst: str) -> None:
    src = loading.loadfile(src)
    dst = loading.loadfile(dst)
    patches = list(unpatch.unpatch(src, dst, verbose=True))
    loading.dumpfile(patches, format="json")
