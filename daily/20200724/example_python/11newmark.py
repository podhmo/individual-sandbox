from __future__ import annotations
import typing as t
from functools import partial
from metashape.marker import mark, guess_mark
from handofcats import as_command
from walker import get_walker

as_service = partial(mark, kind="service")


class User:
    name: str


@as_service
class UserService:
    def list_user(self) -> t.List[User]:
        pass


@as_command
def run():
    w = get_walker()
    for cls in w.walk(kinds=["object", "service"]):
        print(guess_mark(cls), cls)
