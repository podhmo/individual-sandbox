from __future__ import annotations
import time
import sys
import subprocess
from handofcats import as_subcommand
import minitask

"""
pipe的なprocess。後々はbidirectionalなprocessを作ることで消える
"""


@as_subcommand
def run(*, keep: bool = False, n: int = 1):
    import pathlib
    import tempfile

    ipc = minitask.IPC()

    with tempfile.TemporaryDirectory() as dirpath:
        pairs = []

        for uid in range(n):
            endpoint = ipc.create_endpoint(pathlib.Path(dirpath) / f"worker.{uid}.fifo")
            endpoint2 = ipc.create_endpoint(
                pathlib.Path(dirpath) / f"worker.{uid}.fifo2"
            )

            sp = subprocess.Popen(
                [sys.executable, __file__, "producer", "--endpoint", endpoint,]
            )
            tp = subprocess.Popen(
                [
                    sys.executable,
                    __file__,
                    "transformer",
                    "--input-endpoint",
                    endpoint,
                    "--output-endpoint",
                    endpoint2,
                ]
            )
            cp = subprocess.Popen(
                [sys.executable, __file__, "consumer", "--endpoint", endpoint2,]
            )
            pairs.append((sp, cp))

        for sp, cp in pairs:
            sp.wait()
            tp.wait()
            cp.terminate()


@as_subcommand
def producer(*, endpoint: str):
    import os

    pid = os.getpid()
    ipc = minitask.IPC()
    with ipc.serve(endpoint) as x:
        for i in range(5):
            x.send("say", kwargs=dict(message="hello", sender=[pid]))
            time.sleep(0.1)


@as_subcommand
def transformer(*, input_endpoint: str, output_endpoint):
    import os

    pid = os.getpid()
    ipc = minitask.IPC()
    with ipc.connect(input_endpoint) as c:
        with ipc.serve(output_endpoint) as s:
            while True:
                req = c.recv()
                if req is None:
                    break
                req.kwargs["message"] = f"@@ {req.kwargs['message']}@@"
                req.kwargs["sender"].append(pid)
                s.send("say", kwargs=req.kwargs)


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
