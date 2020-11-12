# https://www.python-httpx.org/advanced/#calling-into-python-web-apps
# https://www.python-httpx.org/async/#calling-into-python-web-apps

import asyncio
from starlette.applications import Starlette
from starlette.requests import Request
from starlette.responses import HTMLResponse
from starlette.routing import Route


async def hello(request: Request):
    return HTMLResponse("Hello World!")


app = Starlette(routes=[Route("/", hello)])


async def run():
    import httpx

    async with httpx.AsyncClient(app=app, base_url="http://testserver") as client:
        print("----------------------------------------")
        r = await client.get("/")
        print(r, r.text)
        print("----------------------------------------")


asyncio.run(run())
