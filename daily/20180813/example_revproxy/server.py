import json
from wsgiref.simple_server import make_server


def app(environ, start_response):
    status = '200 OK'
    headers = [('Content-type', 'application/json; charset=utf-8')]
    start_response(status, headers)
    data = {
        "name": "foo",
        "age": "20",
    }
    return [json.dumps(data).encode("utf-8")]


def main(port=4445):
    with make_server('', port, app) as httpd:
        httpd.serve_forever()


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--port", type=int, default=4444)
    args = parser.parse_args()
    main(port=args.port)
