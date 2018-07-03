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


def main():
    port = find_free_port()
    httpd = make_server('', port, app)

    # th = threading.Thread(target=httpd.serve_forever, daemon=True)
    th = threading.Thread(target=httpd.handle_request, daemon=True)
    logger.info("Serving on port %s...", port)
    th.start()

    payload = [1, 2]
    response = requests.post(f"http://localhost:{port}", json=payload)
    data = response.json()
    logger.info("response status=%s, data=%s", response.status_code, data)
    th.join()


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    main()
