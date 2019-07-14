from __future__ import annotations
from handofcats import as_command


@as_command
def run(filename: str) -> None:
    print("yay", filename)
