import asyncio
from aioknife.synclike import Group
# https://qiita.com/icoxfog417/items/07cbf5110ca82629aca0

Seconds = [("first", 5), ("second", 0), ("third", 3)]


async def sleeping(order, seconds, hook=None):
    await asyncio.sleep(seconds)
    if hook:
        hook(order)
    return order


def notify(order):
    print(f"{order} has just finished.")


g = Group()
for s in Seconds:
    g.go(sleeping, *s, hook=notify)
for r in g.wait():
    print(f"asyncio.gather result: {r}")
