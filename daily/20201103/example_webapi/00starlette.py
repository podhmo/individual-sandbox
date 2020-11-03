import json
from starlette.applications import Starlette
from starlette.responses import JSONResponse, Response
from starlette.requests import Request
from starlette.routing import Route
from starlette.endpoints import HTTPEndpoint
from starlette.exceptions import HTTPException

TODOS = []


class Todos(HTTPEndpoint):
    async def post(self, request: Request) -> Response:
        try:
            payload = await request.json()
        except json.JSONDecodeError as e:
            raise HTTPException(status_code=400, detail=json.dumps({"message": str(e)}))
        TODOS.append(payload)
        return JSONResponse(TODOS)


routes = [
    Route("/todos", Todos),
]
app = Starlette(debug=True, routes=routes)
