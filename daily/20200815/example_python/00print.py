import asyncio
from handofcats import as_command


async def do_task(uid: int):
    for i in range(10):
        print(f"{uid}: {i}")
        await asyncio.sleep(0.1)


@as_command
def run():
    async def run():
        await asyncio.wait([do_task(i) for i in range(3)])

    asyncio.run(run(), debug=True)
