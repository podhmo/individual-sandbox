import time
import asyncio
import logging

import lib
logger = logging.getLogger(__name__)



async def tick(d):
    while True:
        logger.info("--------------------tick %s--------------------", d)
        await asyncio.sleep(d)


class Crawler:
    def __init__(self, concurrency=None):
        self.running_count = 0
        self._call_count = 0
        self.cache = {}  # domain -> consumer

        self.total_request = 0
        self.start_time = time.time()
        self.semaphore = None
        if concurrency is not None:
            self.semaphore = asyncio.Semaphore(concurrency)

    def crawl(self, request, fn, *args, **kwargs):
        self.running_count += 1
        self.total_request += 1

        domain = request.domain
        consumer = self.cache.get(domain)
        if consumer is None:
            self.cache[domain] = consumer = self.create_consumer(domain)
        fut = asyncio.ensure_future(self._call(consumer, request, fn, *args, **kwargs))
        fut.add_done_callback(self._dec_count)
        return fut

    async def _call(self, consumer, request, fn, *args, **kwargs):
        if self.semaphore:
            async with self.semaphore:
                logger.info("** num of requests *** %s ***", self._call_count)
                self._call_count += 1
                return await consumer(request, fn, *args, **kwargs)
        else:
            self._call_count += 1
            return await consumer(request, fn, *args, **kwargs)

    def _dec_count(self, fut):
        self._call_count -= 1
        self.running_count -= 1

    def create_consumer(self, domain):
        return Consumer(domain)

    def is_running(self):
        return self.running_count > 0

    def on_finished(self):
        logger.info("end: takes %s, total request = %s", time.time() - self.start_time, self.total_request)


class Consumer:
    def __init__(self, domain, limit=3):
        self.domain = domain
        self.semaphore = asyncio.Semaphore(limit)

        self.limit = limit
        self.delta = 1
        self.waittime = 1

        self.count = 0
        self.last_called_at = None

    async def __call__(self, request, fn, *args, **kwargs):
        async with self.semaphore:
            logger.info("c request: %s args=%s", self.domain, request.args)
            t = time.time()
            if self.count <= self.limit and self.last_called_at and (t - self.last_called_at) < self.delta:
                await self.wait()
            self.last_called_at = t
            self.count += 1
            result = await fn(request, *args, **kwargs)
            self.count -= 1
            return result

    async def wait(self):
        logger.info("c *wait**: %s, waittime=%s", self.domain, self.waittime)
        await asyncio.sleep(self.waittime)


async def do_loop(requests, inq, outq, crawler):
    outq.put_nowait([crawler.crawl(req, fn=lib.mock_fetch) for req in requests])
    while crawler.is_running():
        if not outq.empty():
            futs = await outq.get()
            if futs:
                finished, not_finished = await asyncio.wait(futs, return_when=asyncio.FIRST_COMPLETED)
                for fut in finished:
                    inq.put_nowait(fut.result())
                outq.put_nowait(not_finished)
        if not inq.empty():
            returned_requests = (await inq.get()).get_links()
            futs = [crawler.crawl(req, fn=lib.mock_fetch) for req in returned_requests]
            if futs:
                outq.put_nowait(futs)


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()

    inq = asyncio.Queue()
    outq = asyncio.Queue()
    crawler = Crawler(concurrency=4)

    # # ticker
    # asyncio.ensure_future(tick(1))
    from mock_long_requests import requests
    loop.run_until_complete(do_loop(requests, inq, outq, crawler))
    crawler.on_finished()
    loop.close()
