import pathlib
import importlib.util
from dictknife import loading
from prestring.python import Module

spec = importlib.util.find_spec("botocore")
path = pathlib.Path(spec.origin)
if path.name == "__init__.py":
    path = path.parent
d = loading.loadfile(path / ("data/sqs/2012-11-05/service-2.json"))

"""
operations:
  <name>:
    name: <>
    input: {"shapee": <>}
    output: {"shape": <>,  "resultWrapper": <>}
    errors: {}
    documentation
"""

m = Module()
m.from_("__future__").import_("annotations")
m.sep()
m.stmt("# operations")
with m.class_("SQS"):
    for name, sd in d["operations"].items():
        with m.def_(
                name,
                f"input: {sd['input']['shape']}",
                return_type=sd["output"]["shape"] if "output" in sd else None,
        ):
            m.stmt("...")
m.stmt("# shapes")
with m.class_("SQS"):
    for name, sd in d["shapes"].items():
        with m.class_(name):
            # structure, type
            m.stmt("pass")

print(m)
