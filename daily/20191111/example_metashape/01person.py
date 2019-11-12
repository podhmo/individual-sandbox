from __future__ import annotations
from typing import List
from metashape.runtime import get_walker


class Person:
    name: str
    parents: List[Person]


for cls in get_walker(aggressive=True).walk():
    print(cls)
