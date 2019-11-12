from __future__ import annotations
from typing import List
from metashape.runtime import get_walker


class Person:
    name: str
    parents: List[Person]


# broken (here is omit)
for cls in get_walker(aggressive=True, here=__name__).walk():
    print(cls)
