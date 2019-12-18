import time
import asyncio


async def hello(*, debug: bool):
    print("hello", time.time())
    await asyncio.sleep(0.5)
    print("bye", time.time())
