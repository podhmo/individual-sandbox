import asyncio
from async_asgi_testclient import TestClient
from dictknife import loading
from main import app


debug = True


async def run() -> None:
    async with TestClient(app) as client:
        response = await client.get("/openapi.json")
        loading.dumpfile(response.json())


asyncio.run(run(), debug=debug)
