import json
from wsgiref.simple_server import make_server
import typing as t
from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from wsgiref.types import (  # noqa
        WSGIApplication,
        WSGIEnvironment,
        StartResponse,
    )

_exc_info = t.Tuple[t.Optional[t.Type[BaseException]],
                    t.Optional[BaseException],
                    t.Optional[t.TracebackType],
                    ]


def app(
    environ: "WSGIEnvironment",
    start_response: t.Union[t.Callable[
        [t.Text, t.List[t.Tuple[t.Text, t.Text]]], t.Callable[[bytes], None]
    ], t.Callable[[t.Text, t.List[t.Tuple[t.Text, t.Text]], _exc_info], t.Callable[[bytes], None]]],
) -> t.Iterable[bytes]:
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
