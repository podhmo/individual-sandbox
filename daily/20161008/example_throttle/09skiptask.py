import random
import asyncio
import logging

import lib
logger = logging.getLogger(__name__)


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


class RecQueue:
    def __init__(self, q):
        self.sc = 0
        self.rc = 0
        self.ec = 0
        self.q = q

    def add(self, coro):
        self.sc += 1
        self.rc += 1
        fut = asyncio.ensure_future(coro)
        fut.add_done_callback(self.done_callback)
        return fut

    async def get(self):
        return await self.q.get()

    def empty(self):
        return self.q.empty()

    def done_callback(self, fut):
        self.ec += 1
        self.rc -= 1
        try:
            self.q.put_nowait(fut.result())
        except Exception:
            # retry ?
            pass

    async def join(self):
        while not (self.sc == self.ec and self.rc == 0):
            await asyncio.sleep(0.2)


async def do_loop(requests):
    dispatcher = lib.LimitterDispatcher(lambda request: request.domain, limit_count=2)
    rq = RecQueue(asyncio.Queue())
    for request in requests:
        rq.add(dispatcher.dispatch(fetch, request))

    await rq.join()
    while not rq.empty():
        response = await rq.get()
        for request in response.get_links():
            rq.add(dispatcher.dispatch(fetch, request))
        await rq.join()
    print("yay", rq.ec)


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()
    R = MockRequest
    requests = [
        R("http://sample.y.net/", 1),
        R("http://sample.y.net/", 2),
        R("http://sample.y.net/", 11).add_link(R("http://sample.y.net/z", 1)).add_link(R("http://sample.y.net/z", 2)),
        R("http://sample.y.net/", 1),
        R("http://sample.y.net/", 2),
        R("http://sample.y.net/", 1),
        R("http://sample.y.net/", 2),
        R("http://sample.a.net/", 1),
        R("http://sample.b.net/", 1),
        R("http://sample.c.net/", 1),
        R("http://sample.d.net/", 1),
        R("http://sample.e.net/", 1),
        R("http://sample.f.net/", 1),
        R("http://sample.g.net/", 1),
        R("http://sample.h.net/", 1),
        R("http://sample.i.net/", 1),
        R("http://sample.y.net/", 1),
        R("http://sample.y.net/", 2),
    ]
    # random.shuffle(requests)
    loop.run_until_complete(do_loop(requests))
    loop.close()
