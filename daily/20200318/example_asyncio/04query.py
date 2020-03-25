import minidb
import asyncio


class Users(minidb.AsyncTable):
    pk = "name"


users = Users([{"name": "foo"}, {"name": "bar"}, {"name": "boo"}])


async def run():
    return await users.find_all(in_=["foo", "boo"])


got = asyncio.run(run())
print(got)
# [{'name': 'foo'}, {'name': 'boo'}]
