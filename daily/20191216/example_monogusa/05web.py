from __future__ import annotations
import typing as t
from fastapi import FastAPI, Depends
from pydantic import BaseModel
import monogusa.web.runtime as web


app = FastAPI()


class HelloInput(BaseModel):
    name: str


def database_url() -> str:
    return "sqlite:///:memory:"


class DB:
    def __init__(self, url) -> None:
        self.url = url

    def save(self, message: str) -> None:
        print(f"save {message} in {self.url}")


def db(database_url=Depends(database_url)) -> DB:
    return DB(database_url)


@app.post("/hello", response_model=web.CommandOutput)
def hello(input: HelloInput, db: DB = Depends(db)) -> t.Dict[str, t.Any]:
    with web.handle() as s:
        db.save("hello message")
        return s.dict()


if __name__ == "__main__":
    from monogusa.web import cli

    cli.run(app)
