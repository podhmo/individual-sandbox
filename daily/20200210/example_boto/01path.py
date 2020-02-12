import pathlib
import glob
import importlib.util

spec = importlib.util.find_spec("botocore")
path = pathlib.Path(spec.origin)
if path.name == "__init__.py":
    path = path.parent

for name in glob.glob(f"{path / 'data'}/**/service*.json", recursive=True):
    print(name)
    # print(name.replace(str(path), ""))
