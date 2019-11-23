from starlette.applications import Starlette
from starlette.responses import JSONResponse
from starlette.routing import Route


async def hello(request):
    return JSONResponse({"hello": "world"})


app = Starlette(debug=True, routes=[Route("/", hello)])
