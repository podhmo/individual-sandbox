from dictknife import loading
from handofcats import as_command


@as_command
def run(url: str) -> None:
    rows = loading.loadfile(url, format="spreadsheet")
    loading.dumpfile(rows)
