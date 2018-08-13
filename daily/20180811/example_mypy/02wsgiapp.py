import typing as t
from wsgiref.simple_server import make_server
if t.TYPE_CHECKING:
    from wsgiref.types import StartResponse, WSGIEnvironment  # noqa


def app(
    environ: "WSGIEnvironment",
    start_response: "StartResponse",
) -> t.Iterable[bytes]:
    start_response = t.cast(
        t.Callable[[str, t.List[t.Tuple[str, str]]], t.Callable[[bytes], None]],
        start_response,
    )
    start_response('200 OK', [('Content-type', 'application/json')])
    return [b"ok"]


if __name__ == "__main__":
    port: int = 5000
    with make_server('', port, app) as httpd:
        httpd.serve_forever()
