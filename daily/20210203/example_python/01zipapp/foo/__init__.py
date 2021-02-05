import sys
import json
from wsgiref.simple_server import make_server


def make_app(handler):
    def app(environ, start_response):
        status = "200 OK"
        headers = [("Content-type", "application/json; charset=utf-8")]
        start_response(status, headers)
        return [json.dumps(handler(environ)).encode("utf-8")]

    return app


def run_app(app, port):
    httpd = make_server("", port, app)
    print(f"Serving on port {port}...", file=sys.stderr)

    # Serve until process is killed
    httpd.serve_forever()
