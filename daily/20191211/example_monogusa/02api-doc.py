import asyncio
import os
import magicalimport
from async_asgi_testclient import TestClient
from dictknife import loading


async def main() -> None:
    app = magicalimport.import_symbol("01api.py:app", here=__file__)
    async with TestClient(app) as client:
        response = await client.get("/openapi.json")
        loading.dumpfile(response.json())


if __name__ == "__main__":
    asyncio.run(main(), debug=bool(os.environ.get("DEBUG")))
