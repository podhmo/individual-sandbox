import asyncio
import logging
import time
from collections import ChainMap


class AsyncHandler:
    def __init__(self, loop):
        self.loop = loop

    async def hello(self, name: str):
        t = time.time()
        print(name, "hello")
        await asyncio.sleep(1, loop=self.loop)
        return {"time": time.time() - t}

    def hello2(self, name: str):
        t = time.time()
        print(name, "hello2")

        def cont():
            time.sleep(1)
            print(name, "bye2")
            return {"time2": time.time() - t}

        return cont


class AsyncExecutor:
    def __init__(self, loop, consumer, *, queue=None, max_workers=50):
        self.loop = loop
        self._consumer = consumer
        self.queue = queue or asyncio.Queue()
        self.max_workers = max_workers
        self.workers = []

    def shutdown(self):
        self.loop.run_until_complete(self.queue.join())
        for w in self.workers:
            w.cancel()

    def _create_worker(self, i):
        async def worker(i):
            while True:
                msgid, fn, params = await self.queue.get()

                err = None
                r = None
                try:
                    r = fn(**params)
                    if asyncio.iscoroutine(r):
                        r = await r
                    elif callable(r):
                        r = await self.loop.run_in_executor(None, r)
                except Exception as e:
                    err = e
                self._consumer(msgid, r, error=err)
                self.queue.task_done()

        return asyncio.ensure_future(worker(i), loop=self.loop)

    def execute(self, msgid, fn, params):
        if len(self.workers) <= self.max_workers:
            self.workers.append(self._create_worker(len(self.workers)))
        self.queue.put_nowait((msgid, fn, params))


class AsyncEndpoint:
    def __init__(self, handler, consumer):
        self.handler = handler

        def _consume(msgid, result, error=None):
            d = {"id": msgid}
            if error is None:
                d["result"] = result
            else:
                d["error"] = error
            consumer(d)

        self.executor = AsyncExecutor(handler.loop, _consume)

    def shutdown(self):
        self.executor.shutdown()

    def consume(self, message: dict):
        # todo: validation
        fn = getattr(self.handler, message["method"])
        self.executor.execute(message.get("id"), fn, message["params"])

    def __enter__(self):
        return self

    def __exit__(self, x, y, z):
        self.shutdown()


logging.basicConfig(level=logging.DEBUG)
loop = asyncio.get_event_loop()
handler = AsyncHandler(loop)


def consume(r):
    print("!", r)


message = {
    "jsonrpc": "2.0",
    "method": "hello",
    "params": {
        "name": "foo",
    },
}

with AsyncEndpoint(handler, consume) as endpoint:
    endpoint.consume(message)
    endpoint.consume(message)
    endpoint.consume(message)
    endpoint.consume(message)
    endpoint.consume(ChainMap({"method": "hello2"}, message))
    endpoint.consume(ChainMap({"method": "hello2"}, message))
    endpoint.consume(ChainMap({"method": "hello2"}, message))
    endpoint.consume(ChainMap({"method": "hello2"}, message))
