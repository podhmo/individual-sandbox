import subprocess
import sys
from handofcats import as_subcommand
from findport import free_port


@as_subcommand
def serve(*, port: int, host: str = "") -> None:
    from wsgiref.simple_server import make_server

    def app(environ, start_response):
        status = "200 OK"
        headers = [("Content-type", "application/json; charset=utf-8")]
        start_response(status, headers)
        return [b'{"message": "ok"}']

    server = make_server(host, port, app)
    print("init")
    server.handle_request()
    print("ok")


@as_subcommand
def run() -> None:
    port = free_port()
    p = subprocess.Popen([sys.executable, __file__, "serve", "--port", str(port)])
    import time
    import requests

    time.sleep(0.5)
    res = requests.get(f"http://localhost:{port}")
    res.raise_for_status()
    print(res.json())
    p.wait()


as_subcommand.run()
