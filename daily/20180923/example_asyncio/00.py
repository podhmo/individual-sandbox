import asyncio
from aioknife.synclike import Group


# https://qiita.com/castaneai/items/9cc33817419896667f34
async def func(name, n):
    for i in range(n):
        print(f"func{name}")
        await asyncio.sleep(1)
    return n


g = Group()
g.go(func, "1", 3)
g.go(func, "2", 3)
print(g.wait())
