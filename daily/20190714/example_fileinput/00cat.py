import typing as t
import fileinput
from handofcats import as_command


@as_command
def run(files: t.List[str]) -> None:
    with fileinput.FileInput(files=files) as rf:
        for line in rf:
            print(line, end="")
