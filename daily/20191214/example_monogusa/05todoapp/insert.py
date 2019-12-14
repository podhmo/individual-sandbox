import asyncio
from databases import Database
from models import metadata

import crud

DATABASE_URL = "sqlite:///test.db"
db = Database(DATABASE_URL)


def init_db(metadata):
    import sqlalchemy

    # init table
    engine = sqlalchemy.create_engine(
        DATABASE_URL, connect_args={"check_same_thread": False}
    )
    metadata.create_all(bind=engine)
    engine.dispose()


async def run():
    init_db(metadata)
    await db.connect()
    await crud.create_note(text="hello")


asyncio.run(run(), debug=True)
