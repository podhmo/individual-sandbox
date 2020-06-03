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

    with make_server(host, port, app) as server:
        print("init")
        import signal

        def shutdown(signum, tb):
            print("!!", signum)
            server.shutdown()
            print("server shutdown")

        signal.signal(signal.SIGTERM, shutdown)
        signal.signal(signal.SIGINT, shutdown)
        import threading

        th = threading.Thread(target=server.serve_forever)
        th.start()
        th.join()
    print("process end")


@as_subcommand
def run() -> None:
    port = free_port()
    p = subprocess.Popen([sys.executable, __file__, "serve", "--port", str(port)])
    import time
    import requests

    time.sleep(0.5)
    for i in range(5):
        res = requests.get(f"http://localhost:{port}")
        res.raise_for_status()
        print(res.json())
    p.terminate()  # send SIGTERM
    p.wait()


as_subcommand.run()
