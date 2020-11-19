from __future__ import annotations
import typing as t
import dataclasses
import asyncio
from starlette.applications import Starlette
from starlette.requests import Request
from starlette.routing import Route
from marshmallow import Schema, fields
import httpx
from accessing import Translator

if t.TYPE_CHECKING:
    from accessing import Accessor

# see
# - path
# - queries
# - headers
# - cookies
# - form data
# - body


class UserSchema(Schema):
    name = fields.String(required=True)
    age = fields.Integer()


@dataclasses.dataclass
class RegisterUserInput:
    service_name: str
    user: t.Dict

    @classmethod
    async def parse(cls, a: Accessor) -> RegisterUserInput:
        # check header
        # check queries
        name = a.paths["name"]

        payload = t.cast(t.Mapping[str, t.Any], await a.jsondata())
        user = UserSchema().load(payload)
        return RegisterUserInput(service_name=name, user=user)


async def register(request: Request):
    t = Translator(request)
    input = await t.parse(RegisterUserInput.parse)
    return t.dump_json(UserSchema().dump, input.user)


app = Starlette(
    debug=True, routes=[Route("/services/{name}/users", register, methods=["POST"])],
)


@dataclasses.dataclass
class APIClient:
    client: httpx.AsyncClient
    base_url: str = dataclasses.field(default="http://testserver")

    async def __aenter__(self):
        await self.client.__aenter__()
        return self

    async def __aexit__(self, a, b, c):
        return await self.client.__aexit__(a, b, c)

    @property
    def xxx(self):
        return XXXService(self)


class XXXService:
    def __init__(self, client: APIClient) -> None:
        self.client = client

    async def register(self, uinput: RegisterUserInput):
        data = UserSchema().load(uinput.user)
        url = self.client.base_url + f"/services/{uinput.service_name}/users"
        req = self.client.client.build_request("POST", url, json=data)
        return await self.client.client.send(req)  # auth,allow_redirects,timeout,stream


async def run():
    async with APIClient(client=httpx.AsyncClient(app=app)) as client:
        uinput = RegisterUserInput(service_name="xxx", user={"name": "foo"})
        res = await client.xxx.register(uinput)
        print(res, res.json())


asyncio.run(run())
