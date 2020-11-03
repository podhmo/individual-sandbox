from __future__ import annotations
import typing as t
import json
import dataclasses
from starlette.applications import Starlette
from starlette.responses import JSONResponse, Response
from starlette.requests import Request
from starlette.routing import Route
from starlette.endpoints import HTTPEndpoint
from starlette.exceptions import HTTPException


@dataclasses.dataclass
class Todo:
    title: str


TODOS: t.List[Todo] = []


def add_todo(todo: Todo) -> None:
    TODOS.append(todo)


def list_todo() -> t.List[Todo]:
    return TODOS


async def parse_todo(request) -> Todo:
    try:
        payload = await request.json()
    except json.JSONDecodeError as e:
        raise HTTPException(status_code=400, detail=json.dumps({"message": str(e)}))
    return Todo(**payload)


class Todos(HTTPEndpoint):
    async def post(self, request: Request) -> Response:
        todo = await parse_todo(request)
        add_todo(todo)
        return JSONResponse([dataclasses.asdict(x) for x in list_todo()])


routes = [
    Route("/todos", Todos),
]
app = Starlette(debug=True, routes=routes)
