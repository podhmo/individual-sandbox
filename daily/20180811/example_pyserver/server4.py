import json
import threading
import signal
from wsgiref.simple_server import make_server
from handofcats import as_command


def app(environ, start_response):
    status = '200 OK'
    headers = [('Content-type', 'application/json; charset=utf-8')]
    start_response(status, headers)
    data = {
        "name": "foo",
        "age": "20",
    }
    return [json.dumps(data).encode("utf-8")]


@as_command()
def main(port=4444):
    httpd = make_server('', port, app)
    th = threading.Thread(target=httpd.serve_forever)
    th.start()

    def handle(signum, tb):
        print("*")
        httpd.shutdown()
        print("*")

    signal.signal(signal.SIGTERM, handle)
    signal.signal(signal.SIGINT, handle)
    input()
