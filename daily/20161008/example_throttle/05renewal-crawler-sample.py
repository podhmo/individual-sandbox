import time
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


SENTINEL = object()


class Looping:
    def __init__(self, pipe, fetcher, provider, consumer, concurrency=8):
        self.concurrency = concurrency
        self.pipe = pipe

        self.fetcher = fetcher  # fetcher is Callale[key, fn, args, kwargs] => Coroutine[result]
        self.provider = provider  # provider is AsyncCallable[Looping, Pipe, i]
        self.consumer = consumer  # consumer is AsyncCallable[Looping, Pipe, i]

        self.is_running = False
        self._end_workers = 0
        self._running_futures = 0
        self.total_count = 0
        self.end_count = 0
        self.start_time = time.time()

    async def run_loop(self, requests):
        self.is_running = True
        for i in range(self.concurrency):
            asyncio.ensure_future(self.run_worker(i))

        rpipe = self.pipe.reversed_pipe()

        for request in requests:
            rpipe.write_nowait(request)

        while not rpipe.empty():
            await self.provider(self, rpipe, 0)
        await self.finish(rpipe)

    async def finish(self, rpipe):
        # waiting unfinished workers
        self.is_running = False

        async def teardown():
            logger.info("teardown")
            for i in range(self.concurrency):
                rpipe.write_nowait(SENTINEL)
            while not rpipe.empty() or self.total_count != self.end_count:
                if not rpipe.inq.empty():
                    await self.provider(self, rpipe, 0)
                if not self.pipe.inq.empty():
                    request = self.pipe.read_nowait()
                    if request is not SENTINEL:
                        await self.consumer(self, self.pipe, 0, request)
                await asyncio.sleep(0.2)
        asyncio.ensure_future(teardown())

        while self._end_workers == self.concurrency:
            await asyncio.sleep(0.2)
        while not rpipe.empty() or self.total_count != self.end_count:
            logger.info("empty %s, total=%s, end=%s", rpipe.empty(), self.total_count, self.end_count)
            await asyncio.sleep(0.2)
        logger.info("end: takes %s, total request = %s", time.time() - self.start_time, self.total_count)

    async def run_worker(self, i):
        while self.is_running:
            request = await self.pipe.read()
            if request is SENTINEL:
                break
            await self.consumer(self, self.pipe, i, request)
        self._end_workers += 1

    def run_task(self, key, fn, *args, **kwargs):
        self.total_count += 1
        self._running_futures += 1
        fut = asyncio.ensure_future(self.fetcher(key, fn, args, kwargs))
        fut.add_done_callback(self._dec_count)
        return fut

    def _dec_count(self, fut):
        self._running_futures -= 1
        self.end_count += 1


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()

    def limittable_fetcher(same_origin_limit):
        dispatcher = Dispatcher(limit_count=same_origin_limit)
        cache = {}

        def fetch(domain, fn, args, kwargs):
            fingerprint = (domain, args[0].args)
            if fingerprint in cache:
                fut = asyncio.Future()
                fut.set_result(cache[fingerprint])
                return fut
            else:
                fut = asyncio.ensure_future(dispatcher.dispatch(domain, fn, args, kwargs))
                fut.add_done_callback(lambda fut: add_to_cache(fut, fingerprint))
                return fut

        def add_to_cache(fut, fingerprint):
            if fingerprint not in cache:
                cache[fingerprint] = fut.result()
        return fetch

    async def consume(looping, pipe, i, request):
        logger.info("run consume: (%s)", i)
        response = await looping.run_task(request.domain, lib.mock_fetch, request)
        pipe.write_nowait(response)

    async def provide(looping, pipe, i):
        logger.info("run provide: (%s)", i)
        response = await pipe.read()
        for link in response.get_links():
            pipe.write_nowait(link)

    pipe = lib.Pipe(asyncio.Queue(), asyncio.Queue())
    fetcher = limittable_fetcher(same_origin_limit=3)
    looping = Looping(pipe, fetcher, provide, consume, concurrency=8)

    from mock_long_requests import requests

    loop.run_until_complete(looping.run_loop(requests))
    loop.close()
