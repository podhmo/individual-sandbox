from __future__ import annotations
import typing as t
import typing_extensions as tx
import json
from starlette.requests import Request
from starlette.responses import JSONResponse
from starlette.exceptions import HTTPException
from marshmallow import ValidationError

T = t.TypeVar("T")


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


async def parse(fn: t.Callable[[Accessor], t.Awaitable[T]], request: Request) -> T:
    try:
        return await fn(StaletteAccessor(request))
    except ValidationError as e:
        raise HTTPException(status_code=400, detail=json.dumps(e.normalized_messages()))


class Translator:
    def __init__(self, request: Request) -> None:
        self.request = request

    async def parse(self, fn: t.Callable[[Accessor], t.Awaitable[T]]) -> T:
        try:
            return await fn(StaletteAccessor(self.request))
        except ValidationError as e:
            raise HTTPException(
                status_code=400, detail=json.dumps(e.normalized_messages())
            )

    def dump_json(self, data) -> t.Any:
        return JSONResponse(data)
