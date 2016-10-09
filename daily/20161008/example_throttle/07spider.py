import time
import aiohttp
import asyncio
import logging

import lib
import spiderlib
logger = logging.getLogger(__name__)


async def do_loop(loop):
    cacher = lib.Cacher(lambda url: url)
    dispatcher = lib.LimitterDispatcher(lambda url: spiderlib.urlparse(url).netloc, limit_count=2)

    async with aiohttp.ClientSession(loop=loop) as session:
        async def fetch(url):
            with aiohttp.Timeout(10, loop=session.loop):
                async with session.get(url) as response:
                    text = await response.text()
                    urls = [spiderlib.urljoin(url, spiderlib.remove_fragment(new_url)) for new_url in spiderlib.get_links(text)]
                    return url, urls

        async def consume(supervisor, pipe, i):
            url = await supervisor.pipe.read()
            if url is lib.END_OF_STREAM:
                return
            logger.info("fetch(%s): url=%s", i, url)
            response = await supervisor.consume(cacher, lambda request: dispatcher.dispatch(fetch, request), url)
            pipe.write_nowait(response)

        async def provide(supervisor, pipe, i):
            st = time.time()
            url, urls = await pipe.read()
            logger.info("fetched(%s): url=%s", i, url)
            for u in urls:
                pipe.write_nowait(u)
            logger.info("takes  (%s): %s", i, time.time() - st)

        pipe = lib.Pipe(asyncio.Queue(), asyncio.Queue())
        supervisor = lib.Supervisor(pipe, provide, consume, concurrency=10)
        base_url = 'http://www.tornadoweb.org/en/stable/'
        base_url2 = 'http://python.org/'
        await supervisor.run_loop([base_url, base_url2])

if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop(loop))
    loop.close()
