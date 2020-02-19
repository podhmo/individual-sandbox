import os
from handofcats import as_command
from dictknife import loading
from dictknife import deepmerge


@as_command
def run(*, config: str) -> None:
    c = loading.loadfile(config)

    overwrite_file = os.environ.get("OVERWRITE_CONFIG")
    if overwrite_file is not None:
        c2 = loading.loadfile(overwrite_file)
        c = deepmerge(c, c2, method="replace")
    loading.dumpfile(c)
