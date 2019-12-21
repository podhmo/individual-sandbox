import time
from starlette.applications import Starlette
from starlette.routing import Route
from starlette.requests import Request
from starlette.templating import Jinja2Templates
from starlette.testclient import TestClient
from starlette.middleware.base import BaseHTTPMiddleware


class CustomHeaderMiddleware(BaseHTTPMiddleware):
    async def dispatch(self, request, call_next):
        start_time = time.time()
        response = await call_next(request)
        process_time = time.time() - start_time
        response.headers["X-Process-Time"] = str(process_time)
        return response


templates = Jinja2Templates(directory="templates")


async def read_root(request):
    return templates.TemplateResponse("page.html", {"request": request})


routes = [Route("/", endpoint=read_root)]

app = Starlette(debug=True, routes=routes)
app.add_middleware(CustomHeaderMiddleware)

# use starlette starlette==0.12.9, error
# def test_middleware():
#     client = TestClient(app)
#     response = client.get("/")
#     assert "X-Process-Time" in response.headers
import pytest

@pytest.mark.asyncio

async def test_middleware2():
    from async_asgi_testclient import TestClient

    async with TestClient(app) as client:
        response = await client.get("/")
        assert "X-Process-Time" in response.headers
