import importlib.resources

with importlib.resources.path("vega_datasets", "_data") as path:
    for fpath in path.glob("*.json"):
        print(fpath.name)
