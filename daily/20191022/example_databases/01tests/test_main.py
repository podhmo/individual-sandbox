import unittest
import asyncio
from databases import Database

# TODO: echo


async def create_tables(db):
    q = """
CREATE TABLE IF NOT EXISTS HighScores (
  id INTEGER PRIMARY KEY,
  name VARCHAR(100),
  score INTEGER
)
"""
    await db.execute(query=q)


async def setup_data(db):
    q = """
DELETE FROM HighScores
"""
    await db.execute(query=q)

    q = """
INSERT INTO HighScores(
  name,
  score
) VALUES (
  :name,
  :score
)
"""
    values = [
        {"name": "Daisy", "score": 92},
        {"name": "Neil", "score": 87},
        {"name": "Carol", "score": 43},
    ]
    await db.execute_many(query=q, values=values)


async def fetch_all(db):
    q = """
SELECT * FROM HighScores
"""
    rows = await db.fetch_all(query=q)
    return rows


class Tests(unittest.TestCase):
    def test_it(self):
        async def run():
            # https://github.com/encode/databases/issues/75
            # async with Database("sqlite:///:memory:") as db:

            async with Database("sqlite:///example.db") as db:
                await create_tables(db)
                await setup_data(db)

                got = await fetch_all(db)
                want = [(1, "Daisy", 92), (2, "Neil", 87), (3, "Carol", 43)]
                self.assertListEqual(got, want)

        asyncio.run(run(), debug=True)
