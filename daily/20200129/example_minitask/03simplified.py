import minitask
from handofcats import as_subcommand
from minitask.executor.namedpipe import Executor
from minitask.communication import namedpipe

executor = Executor()


@as_subcommand
def run():
    with executor:
        endpoint = executor.create_endpoint()
        executor.spawn(worker, endpoint=endpoint)

        ipc = minitask.IPC(communication=namedpipe)
        with ipc.serve(endpoint) as x:
            x.send({"method": "add", "args": [1, 2]})
            x.send({"method": "add", "args": [1, 2]})
            x.send({"method": "add", "args": [1, 2]})


@executor.register
def worker(*, endpoint: str):
    ipc = minitask.IPC(communication=namedpipe)

    for item in ipc.connect(endpoint):
        print("<-", item)
        import time

        time.sleep(0.1)
        print("->", sum(item["args"]))


as_subcommand.run()
