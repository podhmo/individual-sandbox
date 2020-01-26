import os
import minitask
from handofcats import as_subcommand
from minitask.executor.namedpipe import Executor
from minitask.port import namedpipe

executor = Executor()


@as_subcommand
def run():
    with executor:
        endpoint = executor.create_endpoint(uid=1)  # portにあるべきでは？
        executor.spawn(reader, endpoint=endpoint)
        executor.spawn(writer, endpoint=endpoint)


@executor.register
def reader(*, endpoint: str):
    ipc = minitask.IPC(port=namedpipe)
    pid = os.getpid()
    with ipc.connect(endpoint) as x:
        for msg in x:
            print(pid, "got", msg.serialize())


@executor.register
def writer(*, endpoint: str):
    ipc = minitask.IPC(port=namedpipe)
    pid = os.getpid()
    with ipc.serve(endpoint) as x:
        for i in range(5):
            print(pid, "send", i)
            x.send(x.serialization.create_message("hello", kwargs=dict(i=i)))


as_subcommand.run()
