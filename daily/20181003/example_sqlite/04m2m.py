import setup
import asyncio
import sqlite3
from pathlib import Path
from typing import Union, Any
import aiosqlite


def connect(database: Union[str, Path], *, loop: asyncio.AbstractEventLoop = None,
            **kwargs: Any) -> aiosqlite.Connection:
    """Create and return a connection proxy to the sqlite database."""
    if loop is None:
        loop = asyncio.get_event_loop()

    def connector() -> sqlite3.Connection:
        conn = sqlite3.connect(str(database), **kwargs)
        conn.set_trace_callback(print)
        conn.row_factory = sqlite3.Row
        return conn

    return aiosqlite.Connection(connector, loop)


async def run():
    async with connect(":memory:") as db:
        for q in setup.QUERIES:
            await db.execute(q)
        async for x in await db.execute("select id, name from xs"):
            async for t in await db.execute(
                "select y_id, memo from ts where x_id = ?", (x["id"], )
            ):
                async for y in await db.execute(
                    "select id, name from ys where id = ?", (t["y_id"], )
                ):
                    print("@", x["id"], y["id"], x["name"], y["name"], t["memo"])


import asyncio
asyncio.get_event_loop().run_until_complete(run())
