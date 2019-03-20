import asyncio
import tqdm


async def do_task():
    for _ in tqdm.tqdm(range(10)):
        await asyncio.sleep(0.3)


async def run():
    await asyncio.wait([do_task(), do_task()])


asyncio.run(run(), debug=True)
