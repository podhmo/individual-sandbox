import typing as t
import typing_extensions as tx
import json
from wsgiref.simple_server import make_server
from wsgiref.util import is_hop_by_hop
import requests


class Request:
    def __init__(self, environ):
        self.environ = environ

    @property
    def wsgi_input(self):
        return self.environ["wsgi.input"]

    @property
    def path(self):
        return self.environ["PATH_INFO"]

    @property
    def method(self):
        return self.environ["REQUEST_METHOD"]

    @property
    def query_string(self):
        return self.environ.get("QUERY_STRING")

    @property
    def headers(self):
        environ = self.environ
        return {k[5:].replace("_", "-"): environ[k] for k in environ if k.startswith("HTTP_")}

    @property
    def content_type(self):
        return self.environ.get("CONTENT_TYPE")

    @property
    def content_length(self):
        v = self.environ.get("CONTENT_LENGTH") or None
        if v is None:
            return None
        return int(v)

    @property
    def data(self):
        if not self.content_length:
            return None
        return self.wsgi_input.read(self.content_length)


class Response(tx.Protocol):
    status_code: int
    reason: str
    headers: t.Dict[str, str]

    @property
    def content(self) -> bytes:
        ...

    def json(self) -> dict:
        ...


class Proxy:
    def __init__(
        self,
        request: t.Callable[[Request], Response],
        response: t.Callable[[Response], bytes],
    ):
        self.request = request
        self.response = response

    def __call__(
        self,
        environ: dict,
        start_response: t.Callable[[str, t.List[t.Tuple[str, str]]], None],
    ) -> None:
        req = Request(environ)
        if req.method == "CONNECT":
            start_response(f'200 OK', [])
            return [b'Connection Established']

        response = self.request(req)
        for k in list(response.headers.keys()):
            if is_hop_by_hop(k):
                response.headers.pop(k)
        if response.status_code == 200:
            content = self.response(response)
        else:
            content = response.content
        start_response(f'{response.status_code} {response.reason}', list(response.headers.items()))
        return [content]


def main(port=4444):
    def request(req: Request) -> Response:
        url = req.path  # client proxyのときにはPATH_INFOにそのまま値が入る
        if req.query_string:
            url = f"{url}?{req.query_string}"
        return requests.request(req.method, url, data=req.data, headers=req.headers)

    def response(res: Response) -> bytes:
        if not res.headers.get("Content-Type", "").lstrip().startswith("application/json"):
            return res.content
        res.headers.pop("Content-Length", None)
        body = res.json()
        for k in list(body.keys()):
            body[k] = f"**{body[k]}**"
        return json.dumps(body).encode("utf-8")

    proxy = Proxy(request, response)
    from wsgiref.simple_server import WSGIRequestHandler as HandlerClass
    HandlerClass.http_version = "1.1"
    with make_server('', port, proxy, handler_class=HandlerClass) as httpd:
        httpd.serve_forever()


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--port", type=int, default=4444)
    args = parser.parse_args()
    main(port=args.port)
