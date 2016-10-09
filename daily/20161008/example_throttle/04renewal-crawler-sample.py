import time
import random
import asyncio
import logging
import lib

logger = logging.getLogger(__name__)


class Limitter:
    def __init__(self, domain, limit=2):
        self.domain = domain
        self.limit = limit
        self.delta = 1
        self.waittime = 1
        self.semaphore = asyncio.Semaphore(limit)

        self.count = 0
        self.last_called_at = None

    async def __call__(self, fn, args, kwargs):
        async with self.semaphore:
            logger.info("L request: %s", self.domain)
            t = time.time()
            self.count += 1
            if self.count >= self.limit and self.last_called_at and (t - self.last_called_at) < self.delta:
                await self.wait()
            self.last_called_at = t
            result = await fn(*args, **kwargs)
            self.count -= 1
            return result

    async def wait(self):
        logger.info("L *wait**: %s, waittime=%s, (%s/%s)", self.domain, self.waittime, self.count, self.limit)
        await asyncio.sleep(self.waittime)


class Dispatcher:
    limitter_factory = Limitter

    def __init__(self, limit_count=2):
        self.cache = {}  # domain -> limitter
        self.limit_count = limit_count

    def dispatch(self, key, fn, args, kwargs):
        limitter = self.cache.get(key)
        if limitter is None:
            self.cache[key] = limitter = self.create_limitter(key)
        return limitter(fn, args, kwargs)

    def create_limitter(self, key):
        return self.limitter_factory(key, limit=self.limit_count)


class Crawler:
    def __init__(self, worker_fn, use_fn, concurrency=8, same_origin_limit=1):
        self.dispatcher = Dispatcher(limit_count=same_origin_limit)
        self.concurrency = concurrency
        self.worker_fn = worker_fn
        self.use_fn = use_fn
        self.pipe = lib.Pipe(asyncio.Queue(), asyncio.Queue())

        self.is_running = False
        self._running_count = 0
        self.total_count = 0
        self.start_time = time.time()

    async def run(self, requests):
        self.is_running = True
        for i in range(self.concurrency):
            asyncio.ensure_future(self.run_worker(i))

        rpipe = self.pipe.reversed_pipe()

        for request in requests:
            rpipe.write_nowait(request)

        while not self.pipe.empty():
            await self.use_fn(self, rpipe, 0)

        self.is_running = False
        # waiting unfinished tasks
        while self._running_count > 0:
            await asyncio.sleep(0.3)
        logger.info("end: takes %s, total request = %s", time.time() - self.start_time, self.total_count)

    async def run_worker(self, i):
        while self.is_running:
            await self.worker_fn(self, self.pipe, i)

    async def get_request(self):
        return await self.q.get()

    def crawl(self, key, fn, *args, **kwargs):
        self.total_count += 1
        self._running_count += 1
        fut = asyncio.ensure_future(self.dispatcher.dispatch(key, fn, args, kwargs))
        fut.add_done_callback(self._dec_count)
        return fut

    def _dec_count(self, fut):
        self._running_count -= 1


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()

    async def worker(crawler, pipe, i):
        logger.info("run worker: (%s)", i)
        request = await pipe.read()
        response = await crawler.crawl(request.domain, lib.mock_fetch, request)
        pipe.write_nowait(response)

    async def consume(crawler, pipe, i):
        logger.info("run consume: (%s)", i)
        response = await pipe.read()
        for link in response.get_links():
            pipe.write_nowait(link)

    crawler = Crawler(worker, consume, concurrency=8, same_origin_limit=3)
    from mock_long_requests import requests
    loop.run_until_complete(crawler.run(requests))
    loop.close()
