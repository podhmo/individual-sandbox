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


def app(environ, start_response):
    status = '200 OK'
    headers = [('Content-type', 'application/json; charset=utf-8')]
    start_response(status, headers)
    wsgi_input = environ["wsgi.input"]
    content_length = int(environ["CONTENT_LENGTH"])
    i, x = json.loads(wsgi_input.read(content_length))
    return [json.dumps([i, x, x * x]).encode("utf-8")]


def generate_stream(L):
    for i, x in L:
        yield i, x


def calculate_stream(itr, *, url):
    for payload in itr:
        response = requests.post(url, json=payload)
        data = response.json()
        logger.info("response status=%s, data=%s", response.status_code, data)
        yield data
    return []


def consume_stream(itr):
    for xs in itr:
        print("@", xs)


def main():
    port = find_free_port()

    httpd = make_server('', port, app)

    th = threading.Thread(target=httpd.serve_forever, daemon=True)
    logger.info("Serving on port %s...", port)
    th.start()

    L = enumerate([1, 2, 3, 4, 5])

    itr = generate_stream(L)
    itr = calculate_stream(itr, url=f"http://localhost:{port}")
    consume_stream(itr)


if __name__ == "__main__":
    logging.basicConfig(level=logging.WARNING)
    main()
