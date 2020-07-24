from __future__ import annotations
import typing as t
import user


class Team:
    name: str
    members: t.List[user.User]
