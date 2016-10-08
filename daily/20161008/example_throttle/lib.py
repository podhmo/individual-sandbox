import time
import asyncio
import logging


logger = logging.getLogger(__name__)


class Cacher:
    def __init__(self, get_key):
        self.get_key = get_key
        self.cache = {}

    def __call__(self, fn, value, *args, **kwargs):
        key = self.get_key(value)
        if key in self.cache:
            fut = asyncio.Future()
            fut.set_result(self.cache[key])
            return fut
        else:
            fut = asyncio.ensure_future(fn(value, *args, **kwargs))
            fut.add_done_callback(lambda fut: self._add_to_cache(fut, key))
            return fut

    def _add_to_cache(self, fut, key):
        if key not in self.cache:
            self.cache[key] = fut.result()


class Limitter:
    def __init__(self, domain, limit=2):
        self.domain = domain
        self.limit = limit
        self.delta = 1
        self.waittime = 1
        self.semaphore = asyncio.Semaphore(limit)

        self.count = 0
        self.last_called_at = None

    async def __call__(self, fn, *args, **kwargs):
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


class LimitterDispatcher:
    limitter_factory = Limitter

    def __init__(self, get_key, limit_count=2):
        self.cache = {}  # domain -> limitter
        self.get_key = get_key
        self.limit_count = limit_count

    def dispatch(self, fn, value, *args, **kwargs):
        key = self.get_key(value)
        limitter = self.cache.get(key)
        if limitter is None:
            self.cache[key] = limitter = self.create_limitter(key)
        return limitter(fn, value, *args, **kwargs)

    def create_limitter(self, key):
        return self.limitter_factory(key, limit=self.limit_count)


END_OF_STREAM = object()


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

    def read_nowait(self):
        return self.inq.get_nowait()

    def write_nowait(self, data):
        return self.outq.put_nowait(data)

    async def write(self, data):
        return await self.outq.put(data)


class Supervisor:
    def __init__(self, pipe, provider, consumer, concurrency=8):
        self.concurrency = concurrency
        self.pipe = pipe

        self.provider = provider  # provider is AsyncCallable[Supervisor, Pipe, i]
        self.consumer = consumer  # consumer is AsyncCallable[Supervisor, Pipe, i]

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
        async def teardown():
            logger.info("teardown")
            while not rpipe.empty() or self.total_count != self.end_count:
                if not rpipe.inq.empty():
                    asyncio.ensure_future(self.provider(self, rpipe, 0))
                if not self.pipe.inq.empty():
                    asyncio.ensure_future(self.consumer(self, self.pipe, 0))
                await asyncio.sleep(0.2)
            logger.info("teardown..")
            for i in range(self.concurrency):
                rpipe.write_nowait(END_OF_STREAM)

        async def _finish():
            # waiting unfinished workers
            logger.info(".. stop worker ..")
            await asyncio.sleep(0.2)
            self.is_running = False

            while self._end_workers == self.concurrency:
                await asyncio.sleep(0.2)
            while not rpipe.empty() or self.total_count != self.end_count:
                logger.info("empty %s, total=%s, end=%s", rpipe.empty(), self.total_count, self.end_count)
                await asyncio.sleep(0.2)

        asyncio.ensure_future(teardown())
        await _finish()
        logger.info("end: takes %s, total request = %s", time.time() - self.start_time, self.total_count)

    async def run_worker(self, i):
        while self.is_running:
            await self.consumer(self, self.pipe, i)
        logger.info("worker %s is end", i)
        self._end_workers += 1

    def consume(self, coro_fn, *args, **kwargs):
        self.total_count += 1
        self._running_futures += 1
        fut = asyncio.ensure_future(coro_fn(*args, **kwargs))
        fut.add_done_callback(self._dec_count)
        return fut

    def _dec_count(self, fut):
        self._running_futures -= 1
        self.end_count += 1
