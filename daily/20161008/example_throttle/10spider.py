import aiohttp
import asyncio
import logging
from html.parser import HTMLParser
from urllib.parse import urljoin, urldefrag, urlparse

import lib
logger = logging.getLogger(__name__)


def remove_fragment(url):
    pure_url, frag = urldefrag(url)
    return pure_url


class URLSeeker(HTMLParser):
    def __init__(self):
        HTMLParser.__init__(self)
        self.urls = []

    def handle_starttag(self, tag, attrs):
        href = dict(attrs).get('href')
        if href and tag == 'a':
            self.urls.append(href)


def get_links(html):
    url_seeker = URLSeeker()
    url_seeker.feed(html)
    return url_seeker.urls


class RecQueue:
    def __init__(self, q):
        self.sc = 0
        self.rc = 0
        self.ec = 0
        self.q = q

    def add(self, coro):
        self.sc += 1
        self.rc += 1
        fut = asyncio.ensure_future(coro)
        fut.add_done_callback(self.done_callback)
        return fut

    def done_callback(self, fut):
        self.ec += 1
        self.rc -= 1
        self.q.put_nowait(fut.result())

    async def join(self):
        while not (self.sc == self.ec and self.rc == 0):
            await asyncio.sleep(0.2)


async def do_loop(urls):
    async with aiohttp.ClientSession(loop=loop) as session:

        async def fetch(url, i=0):
            with aiohttp.Timeout(10, loop=session.loop):
                logger.info("fetch(%s): url=%s", i, url)
                async with session.get(url) as response:
                    text = await response.text()
                    urls = [urljoin(url, remove_fragment(new_url)) for new_url in get_links(text)]
                    logger.info("fetched(%s): url=%s", i, url)
                    return urls

        semaphore = asyncio.Semaphore(10)

        async def access(url):
            async with semaphore:
                return await cacher(lambda url: dispatcher.dispatch(fetch, url), url)

        cacher = lib.Cacher(lambda url: url)
        dispatcher = lib.LimitterDispatcher(lambda url: urlparse(url).netloc, limit_count=2)

        q = asyncio.Queue()
        rq = RecQueue(q)
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
