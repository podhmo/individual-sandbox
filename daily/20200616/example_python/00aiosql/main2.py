import asyncio
import aiosql
import sqlite3
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
queries = aiosql.from_path("queries.sql", "sqlite3", record_classes={"User": User})

print("****************************************", file=sys.stderr)
for name in queries.available_queries:
    print("    ", name, file=sys.stderr)
print("****************************************", file=sys.stderr)


def main():
    conn = sqlite3.connect("greetings.db")
    conn.row_factory = sqlite3.Row

    greetings = queries.get_all_greetings(conn)
    user = queries.get_user_by_username(conn, username="xxx yyy")
    for _, greeting in greetings:
        print(f"{greeting}, {user.username}!")

    # create
    print("!", queries.create_greeting(conn, text="Heh"))


main()
