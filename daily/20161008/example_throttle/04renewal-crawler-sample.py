import time
import random
import asyncio
import logging


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
        self.pipe = Pipe(asyncio.Queue(), asyncio.Queue())

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


class Pipe:
    def __init__(self, inq, outq):
        self.inq = inq
        self.outq = outq

    def reversed_pipe(self):
        return self.__class__(self.outq, self.inq)

    def empty(self):
        return self.inq.empty() and self.outq.empty()

    async def read(self):
        return await self.inq.get()

    def write_nowait(self, data):
        return self.outq.put_nowait(data)

    async def write(self, data):
        return await self.outq.put(data)


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()

    class MockRequest:
        def __init__(self, domain, *args):
            self.domain = domain
            self.args = args
            self.links = []

        def add_link(self, request):
            self.links.append(request)
            return self

        def get_links(self):
            return self.links

    async def fetch(request):
        d = 0.2 * random.random()
        logger.info("R start: %s args=%s, cost=%s", request.domain, request.args, d)
        await asyncio.sleep(d)
        logger.info("R end  : %s arg=%s", request.domain, request.args)
        response = request  # this is dummy, so...
        return response

    R = MockRequest
    requests = [
        R("http://sample.y.net/", 1),
        R("http://sample.y.net/", 2),
        R("http://sample.y.net/", 11).add_link(R("http://sample.y.net/z", 1)).add_link(R("http://sample.y.net/z", 2)).add_link(R("http://sample.y.net/z", 3)),
        R("http://example.x.com/", 1).add_link(R("http://example.x.com/y", 1)).add_link(R("http://example.x.com/y", 2)).add_link(R("http://example.x.com/y", 3)),
        R("http://sample.y.net/", 12),
        R("http://example.x.com/", 2),
        R("http://sample.y.net/", 3),
        R("http://otameshi.z.jp/", 1),
        R("http://example.x.com/", 3),
        R("http://example.x.com/", 1).add_link(R("http://example.x.com/y", 1)).add_link(R("http://example.x.com/y", 2)).add_link(R("http://example.x.com/y", 3)),
        R("http://sample.y.net/", 1),
        R("http://sample.y.net/", 2),
        R("http://sample.y.net/", 11).add_link(R("http://sample.y.net/z", 1)).add_link(R("http://sample.y.net/z", 2)).add_link(R("http://sample.y.net/z", 3)),
        R("http://sample.y.net/", 12),
        R("http://example.x.com/", 2),
        R("http://sample.y.net/", 3),
        R("http://otameshi.z.jp/", 1),
        R("http://example.x.com/", 3),
        R("http://example.x.com/", 1).add_link(R("http://example.x.com/y", 1)).add_link(R("http://example.x.com/y", 2)).add_link(R("http://example.x.com/y", 3)),
        R("http://sample.y.net/", 1),
        R("http://sample.y.net/", 2),
        R("http://sample.y.net/", 11).add_link(R("http://sample.y.net/z", 1)).add_link(R("http://sample.y.net/z", 2)).add_link(R("http://sample.y.net/z", 3)),
        R("http://sample.y.net/", 12),
        R("http://example.x.com/", 2),
        R("http://sample.y.net/", 3),
        R("http://otameshi.z.jp/", 1),
        R("http://example.x.com/", 3),
        R("http://example.x.com/", 1).add_link(R("http://example.x.com/y", 1)).add_link(R("http://example.x.com/y", 2)).add_link(R("http://example.x.com/y", 3)),
        R("http://sample.y.net/", 1),
        R("http://sample.y.net/", 2),
        R("http://sample.y.net/", 11).add_link(R("http://sample.y.net/z", 1)).add_link(R("http://sample.y.net/z", 2)).add_link(R("http://sample.y.net/z", 3)),
        R("http://sample.y.net/", 12),
        R("http://example.x.com/", 2),
        R("http://sample.y.net/", 3),
        R("http://otameshi.z.jp/", 1),
        R("http://example.x.com/", 3),
    ]

    async def worker(crawler, pipe, i):
        logger.info("run worker: (%s)", i)
        request = await pipe.read()
        response = await crawler.crawl(request.domain, fetch, request)
        pipe.write_nowait(response)

    async def consume(crawler, pipe, i):
        logger.info("run consume: (%s)", i)
        response = await pipe.read()
        for link in response.get_links():
            pipe.write_nowait(link)

    crawler = Crawler(worker, consume, concurrency=8, same_origin_limit=3)

    loop.run_until_complete(crawler.run(requests))
    loop.close()
