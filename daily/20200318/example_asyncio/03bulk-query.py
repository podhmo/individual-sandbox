import asyncio
from handofcats import as_command
from minidb import AsyncTable


class Users(AsyncTable):
    pk = "name"


users = Users([{"name": "foo"}, {"name": "bar"}, {"name": "boo"}])


async def run_queries():
    async def do_task(name):
        print(await users.find_one(in_=[name]))

    await asyncio.gather(*[do_task(name) for name in ["foo", "bar", "boo"]])


async def run_bulk_query():
    bulk_fut = asyncio.Future()
    input_buf = []

    async def find_user(name: str):
        input_buf.append(name)
        for row in await bulk_fut:
            if row["name"] == name:
                return row

    async def do_bulk(n):
        await asyncio.sleep(n)
        bulk_fut.set_result(await users.find_all(in_=input_buf))

    async def do_task(name):
        print(await find_user(name))

    actions = [do_task(name) for name in ["foo", "bar", "boo"]]
    actions.append(do_bulk(0.1))
    await asyncio.gather(*actions)


@as_command
def run():
    asyncio.run(run_queries())
    asyncio.run(run_bulk_query())
