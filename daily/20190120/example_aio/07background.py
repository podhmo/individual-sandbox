import asyncio
import logging
import os
from aioknife.synclike.background import Background

logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))


class App(Background):
    async def tick(self, i, d=1):
        print("start", i)
        await asyncio.sleep(d, loop=self.loop)
        return i


b = App(3, interactive=True)
b.add("0", b.tick, 0)
b.add("1", b.tick, 1, d=2)
b.add("2", b.tick, 2)
import time; time.sleep(2)
for r in b:
    print(r)
