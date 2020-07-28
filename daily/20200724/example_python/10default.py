from __future__ import annotations
import typing as t
from handofcats import as_command
from walker import get_fullname, get_walker
import user


class Team:
    name: str
    members: t.List[user.User]


def get_user_by_name(name: str) -> user.User:
    pass


@as_command
def run():
    w = get_walker()
    for x in w.walk(kinds=["callable", "object"]):
        print(get_fullname(x))
