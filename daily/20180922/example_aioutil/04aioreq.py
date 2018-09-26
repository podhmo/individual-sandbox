import aioutil
import time
import aiohttp
import logging
from collections import namedtuple

logging.basicConfig(level=logging.DEBUG)
response = namedtuple("response", "url, status_code")

st = time.time()
g = aioutil.Group()
with g(aiohttp.ClientSession) as session:

    async def do_req(req, *args, **kwargs):
        async with req(*args, **kwargs) as resp:
            return response(url=resp.url, status_code=resp.status)

    g.go(do_req, session.put, 'http://httpbin.org/put', data=b'data')
    g.go(do_req, session.delete, 'http://httpbin.org/delete')
    g.go(do_req, session.head, 'http://httpbin.org/get')
    g.go(do_req, session.options, 'http://httpbin.org/get')
    g.go(do_req, session.patch, 'http://httpbin.org/patch', data=b'data')

for response in g.wait():
    print(response.url, response.status_code)
print(time.time() - st)
