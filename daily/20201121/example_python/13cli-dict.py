from __future__ import annotations
import typing as t
import dataclasses
import json
from handofcats import as_command


@dataclasses.dataclass
class User:
    name: str
    data: t.Dict[str, t.Any]


def load_dict(
    filename_or_string: str,
    *,
    fileprefix: str = "@",
    loads: t.Callable[[str], t.Dict[str, t.Any]] = json.loads,
) -> t.Dict[str, t.Any]:
    if filename_or_string.startswith(filename_or_string):
        filename = filename_or_string[1:]
        with open(filename) as rf:
            s = rf.read()
    else:
        s = filename_or_string
    return loads(s)


@as_command
def run(*, name: str, data: str) -> None:
    name_value = name
    data_value = load_dict(data)
    user = User(name=name_value, data=data_value)
    print("load", user)
