import typing as t
import os
import sys
import subprocess
import selectors
import logging


from handofcats import as_subcommand


"""
subprocessと直接pipeで通信する。stdoutが出力でstdinが入力。
"""

logger = logging.getLogger(__name__)


def send(body: str, *, port: t.IO[str]):
    size = len(body)

    print(size, file=port)
    print(body, file=port, end="")

    logger.debug("send	size:%d	body:%r", size, body)
    port.flush()


def recv(*, port: t.IO[str]) -> str:
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
    sel = selectors.DefaultSelector()

    for uid in range(n):
        p = subprocess.Popen(
            [sys.executable, "-u", __file__, "worker", "--uid", str(uid)],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            text=True,
        )
        ps.append(p)
        sel.register(p.stdout, selectors.EVENT_READ, uid)

    i = 0
    dead = set()

    for p in ps:
        send("hello", port=p.stdin)

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

    def do_task(msg: str):
        for i in range(10):
            print(f"{uid} x{i}", file=sys.stderr)
            sleep(0.1)
        send(f"END", port=sys.stdout)

    msg = recv(port=sys.stdin)
    do_task(msg)


as_subcommand.run()
