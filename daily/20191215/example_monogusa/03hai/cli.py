from __future__ import annotations
from monogusa import component


def hello(db: DB, *, name: str) -> None:
    db.save(f"hello {name}")


class DB:
    def save(self, message: str) -> None:
        print("save: ---", message, "---")


@component
def db() -> DB:
    return DB()
