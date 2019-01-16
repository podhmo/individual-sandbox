from __future__ import annotations
import typing as t
import inspect
import time
import asyncio


class AsyncHandler:
    def __init__(self, loop):
        self.loop = loop

    async def hello(self, name: str):
        t = time.time()
        print(name, "hello")
        await asyncio.sleep(1, loop=self.loop)
        print(name, "bye", time.time() - t)


class AsyncEndpoint:
    def __init__(self, handler, consumer):
        self.handler = handler
        self.consumer = consumer

    @property
    def loop(self):
        return self.handler.loop

    def consume(self, message: t.Dict):
        # todo: validation
        fn = getattr(self.handler, message["method"])

        if inspect.iscoroutinefunction(fn):
            return asyncio.ensure_future(fn(**message["params"]), loop=self.loop)
        else:
            return self.loop.run_in_executor(None, fn, **message["params"])


loop = asyncio.get_event_loop()
handler = AsyncHandler(loop)
endpoint = AsyncEndpoint(handler, consumer=None)
message = {
    "jsonrpc": "2.0",
    "method": "hello",
    "params": {
        "name": "foo",
    },
}
endpoint.consume(message)
loop.run_forever()
