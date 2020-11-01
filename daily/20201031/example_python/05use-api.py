from __future__ import annotations
from handofcats import as_command
import asyncio
import tempfile
import time
import logging
from egoist.ext.serverprocess.lazyparams import find_free_port
from egoist.ext.serverprocess.spawn import spawn_with_connection

logger = logging.getLogger(__name__)


@as_command
def run():
    st = time.time()
    sentinel = tempfile.mktemp()
    with open(sentinel, "w"):
        pass
    port = find_free_port(None)
    cmd = ["uvicorn", "04api:app", "--port", port]
    p, checker = spawn_with_connection(
        cmd, sentinel=sentinel, environ={"SENTINEL": sentinel},
    )

    async def _run():
        import httpx

        async with httpx.AsyncClient() as c:
            r = await c.post(
                f"http://localhost:{port}/dataset/iris/groupby/species/aggs/sepalLength",
                data={"filename": "data/iris/species-sepalLength.json"},
            )
            print(r.text)
            r = await c.post(
                f"http://localhost:{port}/dataset/iris/groupby/species/aggs/sepalWidth",
                data={"filename": "data/iris/species-sepalWidth.json"},
            )
            print(r.text)
            r = await c.post(
                f"http://localhost:{port}/dataset/cars/groupby/Year/aggs/Horsepower",
                data={"filename": "data/cars/Year-Horsepower.json"},
            )
            print(r.text)

    asyncio.run(_run())
    print("ok", time.time() - st)
