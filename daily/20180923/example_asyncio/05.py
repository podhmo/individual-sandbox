import random
import logging
import asyncio
from aioknife.synclike import Group


# https://qiita.com/icoxfog417/items/07cbf5110ca82629aca0
async def sleeping(order, seconds, hook=None):
    await asyncio.sleep(seconds)
    if hook:
        hook(order)
    return order


logging.basicConfig(level=logging.DEBUG)
g = Group(limit=3)
for i in range(9):
    num = random.randint(0, 3)
    g.go(sleeping, str(num), num)
for d in g.wait():
    print(f"limited parallel: {d}")
