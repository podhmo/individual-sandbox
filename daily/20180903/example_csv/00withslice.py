from handofcats import as_command
from dictknife import loading


@as_command
def run(path: str) -> None:
    rows = loading.loadfile(path, format="csv")
    import itertools
    rows = itertools.islice(rows, 3)
    loading.dumpfile(rows, format="json")
