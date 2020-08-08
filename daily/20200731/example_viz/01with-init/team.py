from __future__ import annotations
import typing as t


class Team:
    def __init__(self, name: str, members: t.List[User]) -> None:
        self.name = name
        self.members = members


class User:
    def __init__(self, name: str, age: int) -> None:
        self.name = name
        self.age = age
