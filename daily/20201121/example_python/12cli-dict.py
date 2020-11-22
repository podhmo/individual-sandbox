from __future__ import annotations
import typing as t
import dataclasses
from handofcats import as_command


@dataclasses.dataclass
class User:
    name: str
    data: t.Dict[str, t.Any]


@as_command
def run(*, name: str, data: str) -> None:
    import json

    name_value = name
    with open(data) as rf:
        data_value = json.load(rf)
    user = User(name=name_value, data=data_value)
    print("load", user)
