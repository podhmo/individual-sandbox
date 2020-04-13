# import vega_datasets # error on import panda
import importlib.util
import pathlib

datasets = {}
spec = importlib.util.find_spec("vega_datasets")
for location in spec.submodule_search_locations:
    p = pathlib.Path(location)
    if (p / "_data").exists():
        for f in (p / "_data").glob("*.json"):
            datasets[f.name] = f

print(list(datasets.keys()))
