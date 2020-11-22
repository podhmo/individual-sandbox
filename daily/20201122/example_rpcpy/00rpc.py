import asyncio
import json
import httpx
from rpcpy.application import AsgiRPC

# どうやらJSONRPCの形式には従っていないようだ。


async def add(x: int, y: int) -> int:
    return x + y


rpc = AsgiRPC(openapi={"title": "foo", "description": "-", "version": "0.0.0"})
rpc.register(add)


async def run():
    async with httpx.AsyncClient(app=rpc, base_url="http://testserver") as c:
        data = {"x": 10, "y": 20}
        print("->", data)
        res = await c.post(
            "/add", headers={"content-type": "application/json"}, json=data,
        )
        print("<-", res.json())

        res = await c.get("/get-openapi-docs")
        print(json.dumps(res.json(), indent=2))


asyncio.run(run())
