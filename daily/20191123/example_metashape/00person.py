from metashape.runtime import get_walker
from metashape.outputs.jsonschema import scan, emit


class Person:
    name: str
    age: int


ctx = scan(get_walker(aggressive=True))
import json

print(json.dumps(ctx.result.result, indent=2))
# emit()
