# https://gist.github.com/jordic/378fada474d55bf1722b683486925043
from functools import partial
from imaplib import Int2AP
from starlette.responses import JSONResponse
from starlette.applications import Starlette
from starlette.routing import Route

import httpx
import json
import pydantic as pd
import random


class RPC:
    def __init__(self, name, func, validator):
        self.validator = validator
        self.func = func
        self.name = name

    async def validate(self, data):
        return self.validator(**data)

    def to_schema(self):
        return {"method": self.name, "params": self.validator.schema()}


_services = {}


class rpc:
    def __init__(self, name=None, params=None):
        self.name = name
        self.params = params

    def __call__(self, func):
        name = self.name or func.__name__
        _services[name] = RPC(name, func, self.params)


def error_response(**kwargs):
    return JSONResponse({"jsonrpc": "2.0", "error": kwargs})


async def json_rpc(request):
    try:
        jreq = await request.json()
    except:
        return error_response(code=10000, message="failed_parsing_json")

    rpc = _services.get(jreq["method"], None)

    if rpc is None:
        return error_response(code=10002, message="method_not_found")

    try:
        data = await rpc.validate(jreq["params"])
    except pd.ValidationError as e:
        return error_response(code=10001, message="invalid_data", data=e.errors())

    result = await rpc.func(data)
    return JSONResponse({"jsonrpc": "2.0", "result": result, "id": jreq["id"]})


async def discovery(request):
    return JSONResponse([item.to_schema() for item in _services.values()])


# Example
class SumSchema(pd.BaseModel):
    a: int
    b: int


# That's how we can declare new rpc/services
@rpc(params=SumSchema)
async def sum(data):
    return data.a + data.b


class ClientException(Exception):
    pass


class Client:
    def __init__(self, endpoint, *, app=None):
        self.url = endpoint
        self.endpoints = {}
        self.results = {}
        self.latest = None
        self.app = app

    def _get_client(self) -> httpx.AsyncClient:
        return httpx.AsyncClient(app=self.app, base_url=self.url)

    async def connect(self):
        async with self._get_client() as client:
            res = await client.get("/rpc/@discover")
            endpoints = res.json()
            for item in endpoints:
                self.endpoints[item["method"]] = item

    async def call(self, method, params):
        id_ = str(Int2AP(random.randint(4096, 65535)))
        data = {"jsonrpc": "2.0", "method": method, "params": params, "id": id_}
        async with self._get_client() as client:
            res = await client.post("/rpc", json=data)
            data = res.json()
            self.results[id_] = data
            self.latest = id_
            if "error" in data:
                err = data["error"]
                raise ClientException(
                    f"code={err['code']} message={err['message']}\n{json.dumps(data)}"
                )
            return data["result"]

    def get_latest(self):
        return self.results[self.latest]

    async def caller(self, method, **kwargs):
        return await self.call(method, kwargs)

    def __getattr__(self, name):
        if name in self.endpoints:
            return partial(self.caller, name)


def factory(settings):
    routes = [
        Route("/rpc/@discover", discovery, methods=["GET"]),
        Route("/rpc", json_rpc, methods=["POST"]),
    ]
    app = Starlette(routes=routes)
    return app


## run with uvicorn arpc:app
app = factory({})


async def run():
    async with httpx.AsyncClient(app=app, base_url="http://testserver") as c:
        print("----------------------------------------")
        print("raw")
        print("----------------------------------------")
        data = {
            "jsonrpc": "2.0",
            "method": "sum",
            "params": {"a": 10, "b": 20},
            "id": 1,
        }
        print("->", data)
        res = await c.post(
            "/rpc", json=data, headers={"content-type": "application/json"}
        )
        print("<-", res, res.json())

        print("----------------------------------------")
        print("use client")
        print("----------------------------------------")
        c = Client("http://testserver", app=app)
        await c.connect()
        res = await c.sum(a=10, b=20)
        print("->", {"a": 10, "b": 20})
        print("<-", res)


if __name__ == "__main__":
    import asyncio

    asyncio.run(run())
