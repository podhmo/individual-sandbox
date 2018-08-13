import json
from wsgiref.simple_server import make_server


def app(environ, start_response):
    status = '200 OK'
    headers = [('Content-type', 'application/json')]
    start_response(status, headers)
    data = {
        "msg": "hello world",
    }
    return [json.dumps(data).encode("utf-8")]


if __name__ == "__main__":
    port = 5000
    with make_server('', port, app) as httpd:
        httpd.serve_forever()
