import typing as t
import sys
import string
from metashape.runtime import get_walker
from metashape.outputs.openapi import scan, emit


class Person:
    name: str
    age: int


ctx = scan(get_walker(aggressive=True))


class _Formatter(string.Formatter):
    def __init__(self):
        self.d = {}

    def get_value(self, key: str, args: list, kwargs: dict) -> object:
        self.d[key] = key
        return self.d[key]


def extract_parameters(f, fmt_string) -> t.List[str]:
    f.format(fmt_string)
    r = list(f.d.keys())
    f.d.clear()
    return r


def Path(path: str, method: str, *, response: t.Type[t.Any], _f=_Formatter()) -> object:
    paths = ctx.result.result.get("paths")
    if paths is None:
        paths = ctx.result.result["paths"] = {}
    path_item = paths.get(path)
    if path_item is None:
        path_item = paths[path] = {}
    path_parameters = extract_parameters(_f, path)
    if path_parameters:
        parameters = path_item.get("parameters")
        if parameters is None:
            parameters = path_item["parameters"] = []
            for p in path_parameters:
                parameters.append(
                    {
                        "name": p,
                        "in": "path",
                        "required": True,
                        "schema": {"type": "string"},
                    }
                )
    operation = {method: {"responses": {"200": {"content": {"*/*": {"schema": "<>"}}}}}}
    # description
    # operationID
    # tags
    path_item.update(operation)


paths = [
    Path("/people", "GET", response=t.List[Person]),
    Path("/people/{person_id}", "GET", response=Person),
]
emit(ctx, output=sys.stdout)
