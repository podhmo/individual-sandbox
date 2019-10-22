import logging
import asyncio
from databases import Database

logger = logging.getLogger(__name__)

# https://github.com/encode/databases


async def main() -> None:

    async with Database("sqlite:///example.db") as database:
        # Create a table.
        query = """CREATE TABLE HighScores (id INTEGER PRIMARY KEY, name VARCHAR(100), score INTEGER)"""
        await database.execute(query=query)

        # Insert some data.
        query = "INSERT INTO HighScores(name, score) VALUES (:name, :score)"
        values = [
            {"name": "Daisy", "score": 92},
            {"name": "Neil", "score": 87},
            {"name": "Carol", "score": 43},
        ]
        await database.execute_many(query=query, values=values)

        # Run a database query.
        query = "SELECT * FROM HighScores"
        rows = await database.fetch_all(query=query)
        print("High Scores:", rows)


# logging.basicConfig(level=logging.DEBUG)
logging.basicConfig(level=logging.INFO)
asyncio.run(main(), debug=True)
