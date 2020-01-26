from __future__ import annotations
import time
import sys
import os
import subprocess
from handofcats import as_subcommand
import minitask2 as minitask

"""
bidirectional

"""


@as_subcommand
def run(*, keep: bool = False):
    import pathlib
    import tempfile

    with tempfile.TemporaryDirectory() as dirpath:
        n = 1
        pairs = []

        for uid in range(n):
            input_endpoint = pathlib.Path(dirpath) / f"worker.input.{uid}.fifo"
            output_endpoint = pathlib.Path(dirpath) / f"worker.output.{uid}.fifo"
            sp = subprocess.Popen(
                [
                    sys.executable,
                    __file__,
                    "producer",
                    "--input-endpoint",
                    input_endpoint,
                    "--output-endpoint",
                    output_endpoint,
                ]
            )
            cp = subprocess.Popen(
                [
                    sys.executable,
                    __file__,
                    "consumer",
                    "--input-endpoint",
                    output_endpoint,
                    "--output-endpoint",
                    input_endpoint,
                ]
            )
            pairs.append((sp, cp))

        for sp, cp in pairs:
            sp.wait()
            cp.terminate()


@as_subcommand
def producer(*, input_endpoint: str, output_endpoint: str):
    ipc = minitask.IPC()
    pid = os.getpid()
    with ipc.serve(output_endpoint) as x:
        for i in range(5):
            x.send("say", kwargs=dict(message="hello", sender=[pid]))
            time.sleep(0.1)
    with ipc.connect(input_endpoint) as x:
        while True:
            data = x.recv()
            if data is None:
                break
            print("got", data.unique_id, data.method, data.args, data.kwargs)


@as_subcommand
def consumer(*, input_endpoint: str, output_endpoint: str):
    pid = os.getpid()
    ipc = minitask.IPC()
    with ipc.connect(input_endpoint) as x:
        with ipc.serve(output_endpoint) as y:
            while True:
                data = x.recv()
                if data is None:
                    break
                print("got", data.unique_id, data.method, data.args, data.kwargs)
                y.send(
                    "say2",
                    kwargs={
                        "message": data.kwargs["message"],
                        "sender": [*data.kwargs["sender"], pid],
                    },
                )


as_subcommand.run()
