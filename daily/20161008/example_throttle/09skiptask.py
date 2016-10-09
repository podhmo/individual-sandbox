import asyncio
import logging
import lib
logger = logging.getLogger(__name__)


async def do_loop(requests):
    dispatcher = lib.LimitterDispatcher(lambda request: request.domain, limit_count=2)
    cacher = lib.Cacher(lambda request: (request.domain, request.args))
    rq = lib.RecQueue(asyncio.Queue())
    for request in requests:
        rq.add(cacher(lambda request: dispatcher.dispatch(lib.mock_fetch, request), request))

    await rq.join()
    while not rq.empty():
        response = await rq.get()
        for request in response.get_links():
            rq.add(cacher(lambda request: dispatcher.dispatch(lib.mock_fetch, request), request))
        await rq.join()
    print(vars(rq.q))
    print("yay", rq.ec)


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(message)s", level=logging.DEBUG)
    loop = asyncio.get_event_loop()
    # from mock_long_requests import requests
    from mock_worst_requests import requests
    # random.shuffle(requests)
    loop.run_until_complete(do_loop(requests))
    loop.close()
