import time
import asyncio


async def hello():
    print("start", time.time())
    await asyncio.sleep(0.5)
    print("end", time.time())
