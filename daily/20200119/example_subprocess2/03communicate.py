import typing as t
import sys
import json
import subprocess
import logging
import handofcats
from functools import partial

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
            return EOS, False
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
    logger.info("m")
    import selectors

    s = selectors.SelectSelector()

    cmd = [sys.executable, __file__, "worker"]
    p0 = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
    p1 = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)

    body = json.dumps({"name": "xxx", "age": 20})
    print(f"request {body!r}")
    send(body, port=p0.stdin)
    send(body, port=p0.stdin)
    send(body, port=p1.stdin)
    send(EOS, port=p0.stdin)
    send(EOS, port=p1.stdin)

    s.register(p0.stdout, selectors.EVENT_READ)
    s.register(p1.stdout, selectors.EVENT_READ)

    dead = 0
    while True:
        for k, _ in s.select(None):
            body, status = recv(port=k.fileobj)
            print("@@ response", status, body)
            if not body and not status:
                print(f"!! {body!r} !!")
                dead += 1
                if dead == 2:
                    p0.terminate()
                    p1.terminate()
                    p0.wait()
                    p1.wait()
                    return
                s.unregister(k.fileobj)
                continue


@handofcats.as_subcommand
def worker():
    logger.info("w")

    while True:
        request_body, ok = recv(port=sys.stdin)
        if request_body == EOS:
            send(EOS, ok=False, port=sys.stdout)
            return
        if not ok:
            send("<ignored>", ok=False, port=sys.stdout)
            continue

        try:
            d = json.loads(request_body)
            result = hello(d)
            send(result, port=sys.stdout)
        except Exception as e:
            if VERBOSE:
                logger.warning("error on main", exc_info=True)
            send(str(e), ok=False, port=sys.stdout)


def hello(d: t.Dict[str, t.Any]) -> str:
    return f"hello {d['name']}"


handofcats.as_subcommand.run()
