import typing as t
from databases import Database
from sqlalchemy.engine import Engine
from monogusa import component

from app import crud


def init(engine: Engine) -> None:
    """ init tables"""
    from app.models import metadata

    metadata.create_all(bind=engine)


async def add(db: Database, *, text: str, completed: bool = False) -> t.Mapping:
    await db.connect()  # TODO: startup (lifecycle)
    print(await crud.create_note(db, text=text, completed=completed))


async def list(db: Database) -> t.List[t.Mapping]:
    await db.connect()  # TODO: startup (lifecycle)
    print(await crud.read_notes(db))


@component
def database_url() -> str:
    return "sqlite:///test.db"  # TODO: .env


@component
def db(database_url: str) -> Database:
    return Database(database_url)


@component
def engine(database_url: str) -> Engine:
    import sqlalchemy

    return sqlalchemy.create_engine(
        database_url, connect_args={"check_same_thread": False}
    )
