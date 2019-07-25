import importlib.resources as r
import pathlib


with r.path("foo", "data") as x:
    path = pathlib.Path(x)
    for f in path.glob("*.txt"):
        print(f)
