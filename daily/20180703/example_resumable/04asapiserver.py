import json
import socket
import threading
import logging
import requests
import csv
from contextlib import redirect_stdout
from io import StringIO
from wsgiref.simple_server import make_server
logger = logging.getLogger(__name__)


def find_free_port():
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind(('0.0.0.0', 0))
    port = sock.getsockname()[1]
    sock.close()
    return port


def make_app(consume):
    def app(environ, start_response):
        status = '200 OK'
        headers = [('Content-type', 'application/json; charset=utf-8')]
        start_response(status, headers)
        wsgi_input = environ["wsgi.input"]
        content_length = int(environ["CONTENT_LENGTH"])

        val = json.loads(wsgi_input.read(content_length))
        io = StringIO()
        with redirect_stdout(io):

            class args:
                input = [val]

            consume(args)

        io.seek(0)
        r = csv.DictReader(io)
        result = list(r)
        assert len(result) == 1
        return [json.dumps(result[0]).encode("utf-8")]

    return app


def main():
    import logging
    logging.basicConfig(level=logging.INFO)

    import magicalimport
    handler_main = magicalimport.import_symbol("./03handler_cli.py:main")
    app = make_app(handler_main)

    port = find_free_port()
    httpd = make_server('', port, app)

    logger.info("Serving on port %s...", port)
    httpd.serve_forever()


if __name__ == "__main__":
    main()
