from __future__ import annotations
from monogusa import component


def add(db: DB, *, text: str, completed: bool = False) -> None:
    """
    add notes

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
    """
    db.save({"text": text, "completed": completed})


@component
def db() -> DB:
    return DB()


class DB:
    def save(self, data):
        print("save", data)
