# -*- coding: utf-8 -*-
"""
pep 525 -- asynchronous generators

https://www.python.org/dev/peps/pep-0525
"""
import asyncio


async def aiter(num):
    for i in range(num):
        await asyncio.sleep(0.5)
        yield i


async def run(num):
    async for i in aiter(num):
        print(i)


asyncio.run(run(10), debug=True)
