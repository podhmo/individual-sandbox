import typing as t
import sys
import json
import subprocess
import logging
import handofcats

logger = logging.getLogger(__name__)

"""
protocol

```
{bool} {size}
{body}

{bool} := Tue | False
{size} := <int>
{body} := <str>
```
"""

EOS = ""
VERBOSE = True

def send(body: str, *, ok: bool = True, port: t.IO[str]):
    try:
        print(ok, file=port, end=" ")
        size = len(body)
        print(size, file=port)
        print(body, file=port, end="")
        port.flush()
        logger.debug("send	size:%d	body:%r", size, body)
    except Exception:
        if VERBOSE:
            logger.warning("error on send", exc_info=True)
        raise


def recv(*, port: t.IO[str]) -> t.Tuple[str, bool]:
    try:
        buf = port.readline()
        if buf == "":
            return EOS
        ok, size = buf.split(" ", 1)
        body = port.read(int(size))
        logger.debug("recv	size:%s	body:%r", size, body)
        return body, ok
    except Exception as e:
        if VERBOSE:
            logger.warning("error on recv", exc_info=True)
        return str(e), False


@handofcats.as_subcommand
def main():
    print("m", file=sys.stderr)

    cmd = [sys.executable, __file__, "worker"]
    p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)

    body = json.dumps({"name": "xxx", "age": 20})
    print(f"request {body!r}")
    # send(body[:-1], port=p.stdin)  # xxx
    send(body, port=p.stdin)

    p.wait()
    response_body, ok = recv(port=p.stdout)
    print(f"response ok?={ok}, {response_body!r}")


@handofcats.as_subcommand
def worker():
    print("w", file=sys.stderr)

    # TODO: loop
    request_body, ok = recv(port=sys.stdin)
    if not ok:
        send("<ignored>", ok=False, port=sys.stdout)
        return

    try:
        d = json.loads(request_body)
        result = hello(d)
    except Exception as e:
        if VERBOSE:
            logger.warning("error on main", exc_info=True)
        send(str(e), ok=False, port=sys.stdout)
        return
    send(result, port=sys.stdout)


def hello(d: t.Dict[str, t.Any]) -> str:
    # 1 / 0
    return f"hello {d['name']}"


handofcats.as_subcommand.run()
