from metashape.outputs.openapi import emit
from metashape.runtime import get_walker


class Person:
    name: str


emit(get_walker(aggressive=True))
