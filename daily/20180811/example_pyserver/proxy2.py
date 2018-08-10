import typing as t
import json
from wsgiref.simple_server import make_server
from handofcats import as_command
import requests
from requests.models import Response


class Request:
    def __init__(self, environ):
        self.environ = environ

    @property
    def wsgi_input(self):
        return self.environ["wsgi.input"]

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
        return int(self.environ.get("CONTENT_LENGTH", "0"))

    @property
    def data(self):
        if not self.content_length:
            return None
        return self.wsgi_input.read(self.content_length)


class Proxy:
    def __init__(self, request, response: t.Callable[[Response], bytes]):
        self.request = request
        self.response = response

    def __call__(
        self,
        environ: dict,
        start_response: t.Callable[[str, t.List[t.Tuple[str, str]]], None],
    ) -> None:

        response = self.request(Request(environ))
        if response.status_code == 200:
            content = self.response(response)
        else:
            content = response.content
        start_response(f'{response.status_code} {response.reason}', list(response.headers.items()))
        return [content]


@as_command()
def main(port=4444):
    def request(req: Request) -> Response:
        url = "http://localhost:5000"
        return requests.request(req.method, url, data=req.data, headers=req.headers)

    def response(res: Response) -> bytes:
        if not res.headers.get("Content-Type", "").lstrip().startswith("application/json"):
            return res.content
        body = res.json()
        for k in list(body.keys()):
            body[k] = f"**{body[k]}**"
        res.headers.pop("Content-Length", None)
        return json.dumps(body).encode("utf-8")

    proxy = Proxy(request, response)
    httpd = make_server('', port, proxy)
    httpd.serve_forever()
