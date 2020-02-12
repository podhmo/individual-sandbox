import pathlib
import importlib.util
from dictknife import loading
from dictknife import Accessor
from dictknife import DictWalker

spec = importlib.util.find_spec("botocore")
path = pathlib.Path(spec.origin)
if path.name == "__init__.py":
    path = path.parent
d = loading.loadfile(path / ("data/sqs/2012-11-05/service-2.json"))


dst = {}
a = Accessor(make_dict=dict)

for name, sd in d["operations"].items():
    path = ["operations", name]
    a.assign(dst, path, sd)
    ssd = a.access(d, ["shapes", sd["input"]["shape"]])
    a.assign(dst, ["shapes", sd["input"]["shape"]], ssd)

    if "output" in sd:
        ssd = a.access(d, ["shapes", sd["output"]["shape"]])
        a.assign(dst, ["shapes", sd["output"]["shape"]], ssd)

# slim-up
for path, sd in DictWalker(["documentation"]).walk(dst):
    sd.pop("documentation")
loading.dumpfile(dst, format="json")
