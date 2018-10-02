import asyncio
import aiosqlite
from pathlib import Path

db_path = Path(":memory:")


async def main():
    async with aiosqlite.connect(db_path) as db:
        await db.execute("create table rows (i integer primary key asc, k integer)")
        await db.execute("insert into rows (k) values (2), (3)")
        await db.commit()

        async with db.execute("select last_insert_rowid()") as cursor:
            print("@", await cursor.fetchone())

        async with db.execute("select * from rows") as cursor:
            for row in await cursor.fetchmany():
                print("@", row)


loop = asyncio.get_event_loop()
loop.run_until_complete(main())
