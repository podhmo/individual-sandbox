from metashape.declarative import field
from metashape.outputs.jsonschema import codegen
from metashape.runtime import get_walker


class Value:
    name: str
    name_with_default: str = field("", metadata={"jsonschema": {"maxlength": 255}})
    name_with_metadata_only: str = field(metadata={"jsonschema": {"maxlength": 255}})


w = get_walker(aggressive=True)
codegen(w)
