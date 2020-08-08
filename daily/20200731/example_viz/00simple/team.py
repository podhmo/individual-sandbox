from __future__ import annotations
import typing as t


class Team:
    name: str
    members: t.List[User]


class User:
    name: str
    age: int
