import asyncio
import logging

import lib
logger = logging.getLogger(__name__)


def limited_fetch():
    cacher = lib.Cacher(lambda request: (request.domain, request.args))
    dispatcher = lib.LimitterDispatcher(lambda request: request.domain, limit_count=3)

    def coro_fn(request):
        return dispatcher.dispatch(lib.mock_fetch, request)
        # return cacher(lambda request: dispatcher.dispatch(lib.mock_fetch, request), request)
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
    from mock_long_requests import requests
    loop.run_until_complete(supervisor.run_loop(requests))
    loop.close()
