import json
from pyls_jsonrpc.dispatchers import MethodDispatcher
from wsgiref.simple_server import make_server


class Dispatcher(MethodDispatcher):
    def m_add(self, *, x: int, y: int) -> int:
        return x + y


d = Dispatcher()


def app(environ, start_response):
    global d

    wsgi_input = environ["wsgi.input"]
    content_length = int(environ["CONTENT_LENGTH"])

    data = json.loads(wsgi_input.read(content_length))
    result = {
        "result": d[data["method"]](data["params"]),
        "id": data["id"],
        "jsonrpc": "2.0",
    }

    status = '200 OK'
    body = json.dumps(result).encode("utf-8")
    headers = [
        ('Content-Type', 'application/vscode-jsonrpc; charset=utf-8'),
        ('Content-Length', str(len(body))),
    ]
    start_response(status, headers)
    return [body]


# client
# echo '{"jsonrpc": "2.0", "method": "add", "params": {"x": 10, "y": 20}, "id": 1}' | http -b --json POST :8000
if __name__ == "__main__":
    httpd = make_server("", 8000, app)
    httpd.serve_forever()
