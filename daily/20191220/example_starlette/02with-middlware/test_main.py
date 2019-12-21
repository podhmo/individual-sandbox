from starlette.applications import Starlette
from starlette.middleware import Middleware
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.responses import JSONResponse
from starlette.routing import Route


class CustomHeaderMiddleware(BaseHTTPMiddleware):
    async def dispatch(self, request, call_next):
        response = await call_next(request)
        response.headers["Custom"] = "Example"
        return response


middleware = [Middleware(CustomHeaderMiddleware)]


async def index(request):
    return JSONResponse({"hello": "world"})


routes = [Route("/", endpoint=index)]

app = Starlette(routes=routes, middleware=middleware)

# test code
# test code
def test_app():
    from starlette.testclient import TestClient

    client = TestClient(app)
    response = client.get("/")

    assert response.json() == {"hello": "world"}
    assert "Custom" in response.headers
