import typing as t
from handofcats import as_command
from dictknife import loading
from dictknife import DictWalker


@as_command
def run(filename: str, output: t.Optional[str] = None):
    d = loading.loadfile(filename)
    for path, sd in DictWalker(["allOf"]).walk(d):
        parent = d
        for name in path[:-2]:
            parent = parent[name]
        assert parent[path[-2]] == sd
        parent[path[-2]] = sd.pop("allOf")[0]
    loading.dumpfile(d, output)
