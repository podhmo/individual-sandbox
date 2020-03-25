import minidb
import asyncio


class Users(minidb.AsyncTable):
    pk = "name"


users = Users([{"name": "foo"}, {"name": "bar"}, {"name": "boo"}])


async def run():
    print(await users.find_one(in_=["foo"]))
    # {'name': 'foo'}

    print(await users.find_one(in_=["boo"]))
    # {'name': 'boo'}


asyncio.run(run())
