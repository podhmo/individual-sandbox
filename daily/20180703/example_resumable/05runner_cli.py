import json
import socket
import threading
import logging
import requests
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

        class args:
            input = [val]

        result = list(consume(args))
        assert len(result) == 2
        return [json.dumps(result[1]).encode("utf-8")]

    return app


def main():
    import logging
    logging.basicConfig(level=logging.INFO)

    import magicalimport
    handler_main = magicalimport.import_symbol("./05handler_cli.py:main")
    app = make_app(handler_main)

    port = find_free_port()
    httpd = make_server('', port, app)

    th = threading.Thread(target=httpd.serve_forever, daemon=True)
    logger.info("Serving on port %s...", port)
    th.start()

    rows = [10, 20, 30, 40, 50]
    for row in rows:
        payload = row
        response = requests.post(f"http://localhost:{port}", json=payload)
        data = response.json()
        logger.info("response status=%s, data=%s", response.status_code, data)


if __name__ == "__main__":
    main()
