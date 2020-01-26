from __future__ import annotations
import time
import sys
import subprocess
import itertools
import tempfile
import pathlib
from handofcats import as_subcommand
import minitask2 as minitask


class Executor:
    def __init__(self):
        self.actions = {}
        self._tempdir = None
        self.dirpath = None

    def register(self, fn, name=None):
        self.actions[fn] = name or fn.__name__
        return fn

    def __enter__(self):
        self._tempdir = tempfile.TemporaryDirectory()
        self.dirpath = self._tempdir.__enter__()
        return self

    def __exit__(self, typ, val, tb):
        if self._tempdir is not None:
            return self._tempdir.__exit__(typ, val, tb)

    def spawn(self, fn, **kwargs):
        name = self.actions[fn]
        args = itertools.chain.from_iterable([(f"--{k}", v) for k, v in kwargs.items()])
        return subprocess.Popen([sys.executable, __file__, name, *args])

    def create_endpoint(self, *, uid: int):
        return pathlib.Path(self.dirpath) / f"worker.{uid}.fifo"


@as_subcommand
def run(*, keep: bool = False):
    with Executor() as ex:
        ex.register(consumer)
        ex.register(producer)
        pairs = []
        n = 2

        for uid in range(n):
            endpoint = ex.create_endpoint(uid=uid)
            sp = ex.spawn(producer, endpoint=endpoint)
            cp = ex.spawn(consumer, endpoint=endpoint)
            pairs.append((sp, cp))

        # todo: fix
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
            # msgpackrpc not support kwargs
            x.send("say", args=["hello", [pid]])
            time.sleep(0.1)


@as_subcommand
def consumer(*, endpoint: str):
    ipc = minitask.IPC()
    with ipc.connect(endpoint) as x:
        while True:
            msg = x.recv()
            if msg is None:
                break
            print("got", msg.unique_id, msg.method, msg.args)


as_subcommand.run()
