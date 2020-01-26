from __future__ import annotations
import time
import sys
import subprocess
from handofcats import as_subcommand
import minitask

"""
それっぽく分けてみている

"""


@as_subcommand
def run(*, keep: bool = False):
    import pathlib
    import tempfile

    ipc = minitask.IPC()

    with tempfile.TemporaryDirectory() as dirpath:
        n = 1
        pairs = []

        for uid in range(n):
            endpoint = pathlib.Path(dirpath) / f"worker.{uid}.fifo"
            ipc.create_endpoint(endpoint)

            sp = subprocess.Popen(
                [sys.executable, __file__, "producer", "--endpoint", endpoint]
            )
            cp = subprocess.Popen(
                [sys.executable, __file__, "consumer", "--endpoint", endpoint]
            )
            pairs.append((sp, cp))

        for sp, cp in pairs:
            sp.wait()
            cp.terminate()


@as_subcommand
def producer(*, endpoint: str):
    import os

    ipc = minitask.IPC()
    pid = os.getpid()
    with ipc.serve(endpoint) as x:
        for i in range(5):
            x.send("say", kwargs=dict(message="hello", sender=pid))
            time.sleep(0.1)


@as_subcommand
def consumer(*, endpoint: str):
    ipc = minitask.IPC()
    with ipc.connect(endpoint) as x:
        while True:
            msg = x.recv()
            if msg is None:
                break
            print("got", msg.unique_id, msg.method, msg.args, msg.kwargs)


as_subcommand.run()
