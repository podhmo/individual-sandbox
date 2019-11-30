import sys
from metashape.runtime import get_walker
from metashape.outputs.jsonschema import scan, emit


class Person:
    name: str
    age: int


ctx = scan(get_walker(aggressive=True))
emit(ctx, output=sys.stdout)

