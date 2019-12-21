from starlette.applications import Starlette
from starlette.responses import JSONResponse
from starlette.routing import Route


async def index(request):
    return JSONResponse({"hello": "world"})


routes = [Route("/", endpoint=index)]

app = Starlette(debug=True, routes=routes)


# test code
def test_app():
    from starlette.testclient import TestClient

    client = TestClient(app)
    response = client.get("/")
    assert response.json() == {"hello": "world"}
