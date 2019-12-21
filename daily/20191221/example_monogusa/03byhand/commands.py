from __future__ import annotations
import dataclasses
from monogusa import component


def hello(db: DB, *, name: str = "world") -> None:
    db.save(f"hello {name}")


def byebye(db: DB) -> None:
    db.save("byebye")


@component
def database_url() -> str:
    return "sqlite:///:memory:"


@component
def db(database_url: str) -> DB:
    return DB(database_url)


@dataclasses.dataclass
class DB:
    database_url: str

    def save(self, msg: str) -> None:
        print(f"save: {msg} in {self.database_url!r}")
