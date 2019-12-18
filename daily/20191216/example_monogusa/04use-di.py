from __future__ import annotations
from monogusa import component


def hello(db: DB) -> None:
    db.save("hello message")


@component
def database_url() -> str:
    return "sqlite:///:memory:"


class DB:
    def __init__(self, url) -> None:
        self.url = url

    def save(self, message: str) -> None:
        print(f"save {message} in {self.url}")


@component
def db(database_url: str) -> DB:
    return DB(database_url)
