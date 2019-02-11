from dictknife import loading
from handofcats import as_command


@as_command
def run(*, src: str) -> None:
    data = loading.loadfile(src)
    loading.dumpfile(data, format="yaml")
