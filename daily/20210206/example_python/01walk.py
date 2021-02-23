from __future__ import annotations
import sys
import typing as t
from metashape.runtime import get_walker

Direction = t.Literal["N", "S", "E", "W"]


class A:
    direction: Direction


for cls in get_walker(sys.modules[__name__], recursive=True, aggressive=True).walk():
    print(cls)
