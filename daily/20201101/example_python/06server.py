from __future__ import annotations
import typing as t
import typing_extensions as tx
import json
import dataclasses
from starlette.applications import Starlette
from starlette.requests import Request
from starlette.responses import JSONResponse
from starlette.routing import Route
from starlette.exceptions import HTTPException
from marshmallow import Schema, fields, ValidationError


class Accessor(tx.Protocol):
    @property
    def paths(self) -> GetItem:
        ...

    @property
    def queries(self) -> GetItem:
        ...

    @property
    def headers(self) -> GetItem:
        ...

    @property
    def cookies(self) -> GetItem:
        ...

    async def formdata(self) -> GetItem:
        ...

    async def jsondata(self) -> GetItem:
        ...


class GetItem(tx.Protocol):
    def __getitem__(self, k: t.Any) -> t.Any:
        ...


class GetMultiItem(tx.Protocol):
    def __getitem__(self, k: t.Any) -> str:
        ...

    def getlist(self, key: t.Any) -> t.List[str]:
        ...


class StaletteAccessor:
    def __init__(self, request: Request) -> None:
        self.request = request

    @property
    def paths(self) -> GetItem:
        return self.request.path_params

    @property
    def queries(self) -> GetMultiItem:
        return self.request.query_params

    @property
    def headers(self) -> GetItem:
        return self.request.headers

    @property
    def cookies(self) -> GetItem:
        return self.request.cookies

    async def formdata(self) -> GetMultiItem:
        return await self.request.form()

    async def jsondata(self) -> GetItem:
        try:
            return await self.request.json()
        except json.JSONDecodeError as e:
            raise HTTPException(status_code=400, detail=str(e))


# see
# - path
# - queries
# - headers
# - cookies
# - form data
# - body

T = t.TypeVar("T", covariant=True)


class AsyncParseFunction(tx.Protocol[T]):
    async def __call__(self, a: Accessor) -> T:
        ...


async def parse(fn: AsyncParseFunction[T], request: Request) -> T:
    try:
        return await fn(StaletteAccessor(request))
    except ValidationError as e:
        raise HTTPException(status_code=400, detail=str(e))


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
        return RegisterUserInput(
            service_name=name,
            user=user,
        )


async def register(request: Request):
    input: RegisterUserInput = await parse(RegisterUserInput.parse, request)
    return JSONResponse(UserSchema().dump(input.user))


app = Starlette(
    debug=True,
    routes=[
        Route("/services/{name}/users", register, methods=["POST"]),
    ],
)
