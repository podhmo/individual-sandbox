from __future__ import annotations
import typing as t
from emit import emit


class Team:
    name: str
    members: t.List[User]


class User:
    name: str


print(emit([Team]))
