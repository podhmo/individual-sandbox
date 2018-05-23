import typing
from apistar import App, Route
from apistar.http import Response, HTMLResponse, JSONResponse
from apistar.server.components import ReturnValue
from datetime import datetime


class MyApp(App):
    def render_response(self, return_value: ReturnValue) -> Response:
        if isinstance(return_value, Response):
            return return_value
        elif isinstance(return_value, str):
            return HTMLResponse(return_value)
        return MyJSONResponse(return_value)


class MyJSONResponse(JSONResponse):
    def default(self, obj: typing.Any) -> typing.Any:
        if isinstance(obj, datetime):
            return obj.isoformat()
        return super().default(obj)


def hello() -> dict:
    return {
        "hello": "world",
        "now": datetime.now(),
    }


if __name__ == "__main__":
    routes = [
        Route("/", "GET", hello),
    ]
    app = MyApp(routes=routes)
    app.serve('127.0.0.1', 5000, debug=True)
