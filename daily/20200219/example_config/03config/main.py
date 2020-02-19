from handofcats import as_command
from dictknife import loading


@as_command
def run(*, config: str) -> None:
    c = loading.loadfile(config)
    loading.dumpfile(c)
