import json
from wsgiref.simple_server import make_server
from typing import Iterable
from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from wsgiref.types import (  # noqa
        WSGIApplication,
        WSGIEnvironment,
        StartResponse,
    )


def app(
    environ: "WSGIEnvironment",
    start_response: "StartResponse",
) -> Iterable[bytes]:
    status = '200 OK'
    headers = [('Content-type', 'application/json')]
    start_response(status, headers)
    data = {
        "msg": "hello world",
    }
    return [json.dumps(data).encode("utf-8")]


if __name__ == "__main__":
    port: int = 5000
    with make_server('', port, app) as httpd:
        httpd.serve_forever()
