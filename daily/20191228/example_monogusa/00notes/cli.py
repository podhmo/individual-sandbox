import typing as t
import os
import dotenv
from databases import Database
from sqlalchemy.engine import Engine
from monogusa import component

import crud
from models import metadata


def init(engine: Engine) -> None:
    """ init tables"""
    metadata.create_all(bind=engine)


async def add(db: Database, *, text: str, completed: bool = False) -> None:
    print(await crud.create_note(db, text=text, completed=completed))


async def list(db: Database) -> None:
    for row in await crud.read_notes(db):
        print(dict(row))


@component
def database_url() -> str:
    dotenv.load_dotenv()
    return os.environ["DB_URL"]


@component
async def db(database_url: str) -> Database:
    db = Database(database_url)
    await db.connect()
    return db


@component
def engine(database_url: str) -> Engine:
    import sqlalchemy

    return sqlalchemy.create_engine(
        database_url, connect_args={"check_same_thread": False}
    )
