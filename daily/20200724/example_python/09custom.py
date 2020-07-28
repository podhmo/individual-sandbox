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
    import sys

    builtins = sys.modules["builtins"]

    def guess_kind(x: t.Any) -> t.Optional[t.Literal["object"]]:
        if (
            callable(x)
            and hasattr(x, "__name__")
            and getattr(builtins, x.__name__, None) != x
        ):
            return "object"
        return None

    w = get_walker(guess_kind=guess_kind)
    for x in w.walk(kinds=["callable", "object"]):
        print(get_fullname(x))
