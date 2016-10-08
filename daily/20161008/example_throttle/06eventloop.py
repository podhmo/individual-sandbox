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
    dispatcher = lib.LimitterDispatcher(lambda request: request.domain, limit_count=3)

    async def fetch(request):
        d = 0.2 * random.random()
        logger.info("R start: %s args=%s, cost=%s", request.domain, request.args, d)
        await asyncio.sleep(d)
        logger.info("R end  : %s arg=%s", request.domain, request.args)
        response = request  # this is dummy, so...
        return response

    def coro_fn(request):
        return cacher(lambda request: dispatcher.dispatch(fetch, request), request)
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
    supervisor = lib.Supervisor(pipe, provide, consume, concurrency=8)

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

    loop.run_until_complete(supervisor.run_loop(requests))
    loop.close()
