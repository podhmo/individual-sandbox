import asyncio
from handofcats import as_subcommand


async def _do_task(i):
    print("start")
    await asyncio.sleep(0.5)
    print("end")
    return i


@as_subcommand
def main_list_comprehension(*, n: int):
    async def run():
        return [await _do_task(i) for i in range(n)]

    asyncio.run(run(), debug=True)


@as_subcommand
def main_gather(*, n: int):
    async def run():
        return await asyncio.gather(*[_do_task(i) for i in range(n)])

    asyncio.run(run(), debug=True)


as_subcommand.run()
