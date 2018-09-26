import time
import aiohttp
import logging
import asyncio
from collections import namedtuple

logging.basicConfig(level=logging.DEBUG)
response = namedtuple("response", "url, status_code")


async def run(loop):
    async with aiohttp.ClientSession() as session:

        async def do_req(req, *args, **kwargs):
            async with req(*args, **kwargs) as resp:
                return response(url=resp.url, status_code=resp.status)

        tasks = []
        tasks.append(do_req(session.put, 'http://httpbin.org/put', data=b'data'))
        tasks.append(do_req(session.delete, 'http://httpbin.org/delete'))
        tasks.append(do_req(session.head, 'http://httpbin.org/get'))
        tasks.append(do_req(session.options, 'http://httpbin.org/get'))
        tasks.append(do_req(session.patch, 'http://httpbin.org/patch', data=b'data'))
        return await asyncio.gather(*tasks)


st = time.time()
loop = asyncio.get_event_loop()
for response in loop.run_until_complete(run(loop)):
    print(response.url, response.status_code)
print(time.time() - st)
