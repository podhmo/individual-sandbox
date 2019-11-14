from __future__ import annotations
import typing as t
import sys
from metashape.runtime import get_walker
from metashape.outputs.openapi import emit


class Team:
    name: str
    members: t.List[Person]


class Person:
    team: Team
    name: str
    age: int
    nickname: t.Optional[str]
    parents: t.List[Person]


emit(get_walker(aggressive=True), output=sys.stdout)
