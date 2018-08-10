import typing as t
from wsgiref.simple_server import make_server
from handofcats import as_command
from functools import partial
import requests


def proxy(
    environ: dict,
    start_response: t.Callable[[str, t.List[t.Tuple[str, str]]], None],
    *,
    url: str,
) -> None:
    wsgi_input = environ["wsgi.input"]

    method = environ["REQUEST_METHOD"]
    query_string = environ.get("QUERY_STRING")
    headers = {k[5:].replace("_", "-"): environ[k] for k in environ if k.startswith("HTTP_")}

    content_type = environ.get("CONTENT_TYPE")
    if content_type:
        headers["Content-Type"] = content_type

    content_length = environ.get("CONTENT_LENGTH")
    data = None
    if content_length:
        data = wsgi_input.read(int(content_length))

    if query_string:
        url = f"{url}?{query_string}"

    response = requests.request(method, url, data=data, headers=headers)

    start_response(
        f'{response.status_code} {response.reason}',
        list(response.headers.items()),
    )
    return [response.content]


@as_command()
def main(port=4444):
    httpd = make_server('', port, partial(proxy, url="http://localhost:5000"))
    httpd.serve_forever()
