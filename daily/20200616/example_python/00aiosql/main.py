import asyncio
import aiosql
import aiosqlite
import sys
import logging
from dataclasses import dataclass


@dataclass
class User:
    user_id: int
    username: str
    firstname: str
    lastname: str


logging.basicConfig(level=logging.DEBUG)
queries = aiosql.from_path("queries.sql", "aiosqlite", record_classes={"User": User})
print("****************************************", file=sys.stderr)
for name in queries.available_queries:
    print("    ", name, file=sys.stderr)
print("****************************************", file=sys.stderr)


async def main():
    # Parallel queries!!!
    async with aiosqlite.connect("greetings.db") as conn:
        greetings, user = await asyncio.gather(
            queries.get_all_greetings(conn),
            queries.get_user_by_username(conn, username="xxx yyy"),
        )

        for _, greeting in greetings:
            print(f"{greeting}, {user.username}!")

        # create
        print("!", await queries.create_greeting(conn, text="Heh"))


asyncio.run(main())
