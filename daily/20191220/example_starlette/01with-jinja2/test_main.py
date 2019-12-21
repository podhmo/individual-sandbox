from starlette.applications import Starlette
from starlette.routing import Route
from starlette.templating import Jinja2Templates


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
