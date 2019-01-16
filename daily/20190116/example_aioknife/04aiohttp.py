import os
import asyncio
import logging
import aiohttp
from aioknife.synclike import Executor
logger = logging.getLogger(__name__)


async def fetch(session, url):
    async with session.get(url) as response:
        html = await response.text()
        print(url, response.status, repr(html[:100]))


# todo: middleware?
async def main():
    async with aiohttp.ClientSession() as session:
        ex = Executor()
        ex.submit(fetch, session, 'http://python.org')
        ex.submit(fetch, session, 'http://python.org')
        await ex.run()


logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))
asyncio.run(main())
