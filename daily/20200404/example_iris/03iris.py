import importlib.util
import pathlib
import json

spec = importlib.util.find_spec("vega_datasets")
data = json.load((pathlib.Path(spec.submodule_search_locations[0]) / "_data/iris.json").open())
print(sum([row["petalLength"]for row in data]))
