import aiohttp
import asyncio
import logging


import spiderlib
import lib
logger = logging.getLogger(__name__)


async def do_loop(urls):
    async with aiohttp.ClientSession(loop=loop) as session:

        async def fetch(url, i=0):
            with aiohttp.Timeout(10, loop=session.loop):
                logger.info("fetch(%s): url=%s", i, url)
                async with session.get(url) as response:
                    text = await response.text()
                    urls = [spiderlib.urljoin(url, spiderlib.remove_fragment(new_url)) for new_url in spiderlib.get_links(text)]
                    logger.info("fetched(%s): url=%s", i, url)
                    return urls

        semaphore = asyncio.Semaphore(10)

        async def access(url):
            async with semaphore:
                return await cacher(lambda url: dispatcher.dispatch(fetch, url), url)

        cacher = lib.Cacher(lambda url: url)
        dispatcher = lib.LimitterDispatcher(lambda url: spiderlib.urlparse(url).netloc, limit_count=2)

        q = asyncio.Queue()
        rq = lib.RecQueue(q)
        for url in urls:
            rq.add(access(url))

        await rq.join()
        while not q.empty():
            urls = await q.get()
            for url in urls:
                rq.add(access(url))
            await rq.join()
    print("yay", rq.ec)


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()
    # random.shuffle(requests)
    base_url = 'http://www.tornadoweb.org/en/stable/'
    base_url2 = 'http://python.org/'

    loop.run_until_complete(do_loop([base_url, base_url2]))
    loop.close()
