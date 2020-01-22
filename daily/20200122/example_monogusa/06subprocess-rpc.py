import typing as t
import os
import sys
import subprocess
import selectors
import logging

from handofcats import as_subcommand
from tinyrpc.dispatch import RPCDispatcher
from tinyrpc.protocols import jsonrpc


"""
subprocessと直接pipeで通信する。stdoutが出力でstdinが入力。

- tinyrpcのprotocolsとdispatcherだけを使う
- goも動かせないのなら価値がないのでは？
- responseを期待しないformatってなんだっけ？
- 2段階に分ける必要がある
"""

logger = logging.getLogger(__name__)
protocol = jsonrpc.JSONRPCProtocol()
dispatcher = RPCDispatcher()


@dispatcher.public
def add(x: int, y: int) -> t.Dict[str, t.Any]:
    ans = x + y
    return {"q": f"{x} + {y} = ?", "a": ans}


def send(body: bytes, *, port: t.IO[bytes]):
    size = len(body)

    port.write(str(size).encode("utf-8"))
    port.write(b"\n")
    port.write(body)

    logger.debug("send	size:%d	body:%r", size, body)
    port.flush()


def recv(*, port: t.IO[bytes]) -> bytes:
    size = port.readline()
    if not size:
        return ""
    body = port.read(int(size))
    logger.debug("recv	size:%s	body:%r", size, body)
    return body


@as_subcommand
def manager():
    ps = []
    n = os.cpu_count()
    n = 1
    sel = selectors.DefaultSelector()

    for uid in range(n):
        p = subprocess.Popen(
            [sys.executable, "-u", __file__, "worker", "--uid", str(uid)],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=False,
        )
        ps.append(p)
        sel.register(p.stdout, selectors.EVENT_READ, uid)

    i = 0
    dead = set()

    for p in ps:
        req = protocol.create_request("add", args=[10, 20], kwargs={})
        req = protocol.create_request("do_task")
        send(req.serialize(), port=p.stdin)

    while True:
        for key, events in sel.select():
            uid = key.data
            msg = recv(port=key.fileobj)

            if not msg:
                i += 1
                continue

            if msg == "END":
                if uid is not dead:
                    dead.add(uid)
                    sel.unregister(key.fileobj)
                print("ok", msg, i)
                if len(dead) == n:
                    for p in ps:
                        p.terminate()
                    return


@as_subcommand
def worker(*, uid: int):
    import sys
    from time import sleep

    @dispatcher.public
    def do_task(msg: str):
        for i in range(10):
            print(f"{uid} x{i}", file=sys.stderr)
            sleep(0.1)
        req = protocol.create_request("end", args=[], kwargs={})
        send(req.serialize(), port=sys.stdout.buffer)

    msg = recv(port=sys.stdin)
    protocol.parse_request(msg)
    print("@", msg, file=sys.stderr)
    do_task(msg)


as_subcommand.run()
