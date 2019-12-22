from __future__ import annotations
import dataclasses
from monogusa import component


# on web.py, keyword arguments are aggregated as one schema ( something like InputSchema )
def hello(db: DB, *, name: str = "world") -> None:
    db.save(f"hello {name}")


# on web.py, if function hasn't keyword arguments, skip schema code generation
def byebye(db: DB) -> None:
    db.save("byebye")


# on web.py, zero dependency component is used directly
@component
def database_url() -> str:
    return "sqlite:///:memory:"


# on web.py, generate code for dependency injection
@component
def db(database_url: str) -> DB:
    return DB(database_url)


@dataclasses.dataclass
class DB:
    database_url: str

    def save(self, msg: str) -> None:
        print(f"save: {msg} in {self.database_url!r}")
