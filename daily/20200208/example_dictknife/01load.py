from handofcats import as_command
from dictknife import loading


@as_command
def run(filename: str) -> None:
    d = loading.loadfile(filename)
    print(d)
