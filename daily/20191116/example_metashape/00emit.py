from metashape.runtime import get_walker
from metashape.outputs.openapi import emit


class Person:
    name: str


w = get_walker(aggressive=True)
emit(w)
