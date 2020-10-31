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


app = Starlette(debug=True, routes=[Route("/dataset", DatasetEndpoint)])
