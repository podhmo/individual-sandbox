import time
import os

import minitask
from minitask.executor.namedpipe import Executor
from handofcats import as_subcommand

executor = Executor()
ipc = minitask.IPC()


@executor.register
def watch(*, endpoint: str):
    pid = os.getpid()
    with ipc.connect(endpoint) as x:
        for line in x:
            print("hoi", line, pid)
            time.sleep(0.5)


@as_subcommand
def run():
    with executor:
        endpoint = executor.create_endpoint(uid="xxx")
        executor.spawn(watch, endpoint=endpoint)
        with ipc.serve(endpoint) as x:
            for i in range(10):
                x.send(i)


as_subcommand.run()
