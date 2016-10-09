import time
import asyncio
import logging

import lib
logger = logging.getLogger(__name__)


class Dispatcher:
    def __init__(self):
        self.cache = {}  # domain -> consumer

    def dispatch(self, request, fn, *args, **kwargs):
        domain = request.domain
        consumer = self.cache.get(domain)
        if consumer is None:
            self.cache[domain] = consumer = self.create_consumer(domain)
        return consumer(request, fn, *args, **kwargs)

    def create_consumer(self, domain):
        return Consumer(domain)


class Consumer:
    def __init__(self, domain, limit=2):
        self.domain = domain
        self.semaphore = asyncio.Semaphore(limit)

        self.limit = limit
        self.delta = 1
        self.waittime = 1

        self.count = 0
        self.last_called_at = None

    async def __call__(self, request, fn, *args, **kwargs):
        async with self.semaphore:
            logger.info("c consume: %s args=%s", self.domain, request.args)
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


async def do_loop():
    dispatcher = Dispatcher()
    from mock_mini_requests import requests
    futs = []
    for request in requests:
        fut = dispatcher.dispatch(request, fn=lib.mock_fetch)
        futs.append(fut)

    # まじめに再帰的に処理をするならasyncio.Queueなどが必要
    finished, not_finished = await asyncio.wait(futs)


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop())
