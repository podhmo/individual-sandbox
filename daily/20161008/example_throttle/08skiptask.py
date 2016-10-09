import asyncio
import logging

import lib
logger = logging.getLogger(__name__)


class _RecQueue:
    def __init__(self, cont):
        self.sc = 0
        self.rc = 0
        self.ec = 0
        self.cont = cont

    def add(self, coro):
        self.sc += 1
        self.rc += 1
        fut = asyncio.ensure_future(coro)
        fut.add_done_callback(self.done_callback)
        return fut

    def done_callback(self, fut):
        self.ec += 1
        self.rc -= 1
        coros = self.cont(fut.result())
        if coros is None:
            return

        if not isinstance(coros, (list, tuple)):
            coros = [coros]
        for coro in coros:
            self.add(coro)

    async def join(self):
        while not (self.sc == self.ec and self.rc == 0):
            await asyncio.sleep(0.2)


async def do_loop(requests):
    dispatcher = lib.LimitterDispatcher(lambda request: request.domain, limit_count=2)
    rq = _RecQueue(lambda response: [dispatcher.dispatch(lib.mock_fetch, r) for r in response.get_links()])
    for request in requests:
        rq.add(dispatcher.dispatch(lib.mock_fetch, request))
    await rq.join()


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()
    from mock_long_requests import requests
    # random.shuffle(requests)
    loop.run_until_complete(do_loop(requests))
    loop.close()
