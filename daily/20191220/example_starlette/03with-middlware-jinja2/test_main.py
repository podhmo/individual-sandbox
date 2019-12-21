from starlette.applications import Starlette
from starlette.middleware import Middleware
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.routing import Route
from starlette.templating import Jinja2Templates


class CustomHeaderMiddleware(BaseHTTPMiddleware):
    async def dispatch(self, request, call_next):
        response = await call_next(request)
        response.headers["Custom"] = "Example"
        return response


middleware = [Middleware(CustomHeaderMiddleware)]

templates = Jinja2Templates(directory="templates")


async def homepage(request):
    return templates.TemplateResponse("index.html", {"request": request})


routes = [Route("/", endpoint=homepage)]

app = Starlette(debug=True, routes=routes)

# test code


def test_app():
    from starlette.testclient import TestClient

    client = TestClient(app)
    response = client.get("/")
    response.text == "Hello World!"
