import typing as t
import sys
import json
import subprocess
import logging
import handofcats

logger = logging.getLogger(__name__)


def send(body: str, *, port: t.IO[str]):
    try:
        size = len(body)
        print(size, file=port)
        print(body, file=port, end="")
        port.flush()
        logger.debug("send	size:%d	body:%r", size, body)
    except Exception:
        logger.warning("error on send", exc_info=True)
        raise


def recv(*, port: t.IO[str]) -> str:
    try:
        size = port.readline()
        if size == "":
            return ""  # TODO: send EOF?
        body = port.read(int(size))
        logger.debug("recv	size:%s	body:%r", size, body)
        return body
    except Exception:
        logger.warning("error on recv", exc_info=True)
        raise


@handofcats.as_subcommand
def main():
    print("m", file=sys.stderr)

    cmd = [sys.executable, __file__, "worker"]
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)

    body = json.dumps({"name": "xxx", "age": 20})
    send(body, port=p.stdin)

    p.wait()
    response_body = recv(port=p.stdout)
    print(f"response {response_body!r}")


@handofcats.as_subcommand
def worker():
    print("w", file=sys.stderr)

    # TODO: loop
    request_body = recv(port=sys.stdin)

    d = json.loads(request_body)
    result = hello(d)

    send(result, port=sys.stdout)


def hello(d: t.Dict[str, t.Any]) -> str:
    1 / 0
    return f"hello {d['name']}"


handofcats.as_subcommand.run()
