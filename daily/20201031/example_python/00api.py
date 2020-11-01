from starlette.applications import Starlette
from starlette.requests import Request
from starlette.responses import Response
from starlette.endpoints import HTTPEndpoint
from starlette.routing import Route


class DatasetEndpoint(HTTPEndpoint):
    def get(self, request: Request) -> Response:
        return Response("hello get")

    def post(self, request: Request) -> Response:
        return Response("hello post")


async def on_startup():
    import os
    import sys

    sentinel_file = os.environ.get("SENTINEL")
    if sentinel_file is None:
        print(f"	** sentinel is not found. skip.", file=sys.stderr)
        return
    print(f"	** sentinel is found. removing (ack). {sentinel_file}", file=sys.stderr)
    os.path.exists(sentinel_file) and os.remove(sentinel_file)


routes = [Route("/dataset", DatasetEndpoint)]
app = Starlette(debug=True, routes=routes, on_startup=[on_startup])
