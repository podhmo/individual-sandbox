import asyncio
import logging

import tqdm


async def task(*, desc=None):
    for _ in tqdm.tqdm(range(10), desc=desc):
        await asyncio.sleep(0.5)




async def run():
    await asyncio.wait([task(desc="A"), task(desc="B"), task(desc="C")])


logging.basicConfig(level=logging.DEBUG)
asyncio.run(run(), debug=True)
