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


def limited_fetch():
    cacher = lib.Cacher(lambda request: (request.domain, request.args))
    dispatcher = lib.LimitterDispatcher(lambda request: request.domain, limit_count=2)

    async def fetch(request):
        d = 0.2 * random.random()
        logger.info("R start: %s args=%s, cost=%s", request.domain, request.args, d)
        await asyncio.sleep(d)
        logger.info("R end  : %s arg=%s", request.domain, request.args)
        response = request  # this is dummy, so...
        return response

    def coro_fn(request):
        return dispatcher.dispatch(fetch, request)
        # return cacher(lambda request: dispatcher.dispatch(fetch, request), request)
    return coro_fn


async def consume(supervisor, pipe, i, fetch_function=limited_fetch()):
    request = await supervisor.pipe.read()
    logger.info("run consume: (%s)", i)
    if request is lib.END_OF_STREAM:
        return
    response = await supervisor.consume(fetch_function, request)
    pipe.write_nowait(response)

async def provide(supervisor, pipe, i):
    logger.info("run provide: (%s)", i)
    response = await pipe.read()
    for link in response.get_links():
        pipe.write_nowait(link)


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()

    pipe = lib.Pipe(asyncio.Queue(), asyncio.Queue())
    supervisor = lib.Supervisor(pipe, provide, consume, concurrency=3)

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
    ]

    loop.run_until_complete(supervisor.run_loop(requests))
    loop.close()
