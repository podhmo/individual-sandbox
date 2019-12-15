from __future__ import annotations
from .di import component

def add(db: DB, *, text: str, completed: bool = False) -> None:
    db.save({"text": text, "completed": completed})


@component
def db() -> DB:
    return DB()


class DB:
    def save(self, data):
        print("save", data)
