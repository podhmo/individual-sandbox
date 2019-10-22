import logging
import asyncio
import sqlalchemy
from databases import Database

# https://www.encode.io/databases/database_queries/

metadata = sqlalchemy.MetaData()
logger = logging.getLogger(__name__)

notes = sqlalchemy.Table(
    "notes",
    metadata,
    sqlalchemy.Column("id", sqlalchemy.Integer, primary_key=True),
    sqlalchemy.Column("text", sqlalchemy.String(length=100), nullable=False),
    sqlalchemy.Column("completed", sqlalchemy.Boolean, nullable=False),
)


async def main() -> None:

    async with Database("sqlite:///example.db") as database:
        # Create a table.
        engine = sqlalchemy.create_engine("sqlite:///example.db", echo=True)
        metadata.create_all(bind=engine)

        #         query = """
        # CREATE TABLE IF NOT EXISTS notes (
        #   id INTEGER PRIMARY KEY,
        #   text VARCHAR(100) NOT NULL,
        #   completed BOOLEAN NOT NULL
        # )"""
        #         await database.execute(query=query)

        # Execute
        query = notes.delete()
        await database.execute(query=query)

        # Execute
        query = notes.insert()
        values = {"text": "example1", "completed": True}
        await database.execute(query=query, values=values)

        # Execute many
        query = notes.insert()
        values = [
            {"text": "example2", "completed": False},
            {"text": "example3", "completed": True},
        ]
        await database.execute_many(query=query, values=values)

        query = notes.select()
        rows = await database.fetch_all(query=query)
        print("# Fetch multiple rows")
        print(rows)

        query = notes.select()
        row = await database.fetch_one(query=query)
        print("# Fetch single row")
        print(row)

        query = notes.select()
        value = await database.fetch_val(query=query)
        print("# Fetch single value, defaults to `column=0`.")
        print(value)

        # Fetch multiple rows without loading them all into memory at once
        query = notes.select()
        async for row in database.iterate(query=query):
            print("row", row)


# logging.basicConfig(level=logging.DEBUG)
logging.basicConfig(level=logging.INFO)
asyncio.run(main(), debug=True)
