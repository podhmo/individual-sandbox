import time
import random
import asyncio
import logging


logger = logging.getLogger(__name__)


class Request:
    def __init__(self, domain, *args):
        self.domain = domain
        self.args = args
        logger.info("r init: %s arg=%s", self.domain, self.args)


async def fetch(request):
    d = 1 * random.random()
    logger.info("r start: %s args=%s, cost=%s", request.domain, request.args, d)
    await asyncio.sleep(d)
    logger.info("r end  : %s arg=%s", request.domain, request.args)


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
    requests = [
        Request("x", 1),
        Request("y", 1),
        Request("y", 2),
        Request("y", 11),
        Request("y", 12),
        Request("x", 2),
        Request("y", 3),
        Request("z", 1),
        Request("x", 3),
    ]
    futs = []
    for request in requests:
        fut = dispatcher.dispatch(request, fn=fetch)
        futs.append(fut)

    # まじめに再帰的に処理をするならasyncio.Queueなどが必要
    finished, not_finished = await asyncio.wait(futs)


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()
    loop.run_until_complete(do_loop())
