from __future__ import annotations
from walker import get_walker, get_fullname
from handofcats import as_command


class Zero:
    pass


class X:
    name: str


class Y:
    name: str


class _Private:
    name: str


@as_command
def run():
    w = get_walker()
    for cls in w.walk():
        print(get_fullname(cls))
